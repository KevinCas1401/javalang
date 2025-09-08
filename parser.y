%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <ctype.h>

/* -------- logging -------- */
static int semantic_errors = 0;
static char LOG_BUF[8192];
static size_t LOG_POS = 0;
static void log_reset(){ LOG_POS = 0; LOG_BUF[0] = '\0'; }
static void log_append(const char* msg){
  size_t n = strlen(msg);
  if (LOG_POS + n + 2 < sizeof(LOG_BUF)){
    memcpy(LOG_BUF + LOG_POS, msg, n);
    LOG_POS += n;
    LOG_BUF[LOG_POS++] = '\n';
    LOG_BUF[LOG_POS] = '\0';
  }
}
static int semf(const char* fmt, ...){
  char __tmp[512];
  va_list ap; va_start(ap, fmt);
  vsnprintf(__tmp, sizeof(__tmp), fmt, ap);
  va_end(ap);
  log_append(__tmp);
  semantic_errors++;
  return 0;
}
#define LOGF(...)  do{ char __tmp[512]; snprintf(__tmp,sizeof(__tmp), __VA_ARGS__); log_append(__tmp);}while(0)
#define SEMF(...)  (semf(__VA_ARGS__), 0)

/* -------- Tipos -------- */
typedef enum { TY_UNDEF=0, TY_INT, TY_FLOAT, TY_BOOL, TY_CHAR, TY_STRING, TY_VOID, TY_NULL, TY_ERROR } Type;
static int is_reference(Type t){ return t==TY_STRING; }

/* -------- tabla de símbolos con ENTORNOS -------- */
typedef struct Sym { char* name; Type type; struct Sym* next; } Sym;
typedef struct Scope { Sym* table; struct Scope* parent; } Scope;
static Scope* current_scope = NULL;

static void st_free_list(Sym* p){ while(p){ Sym* n=p->next; free(p->name); free(p); p=n; } }
static void st_push_scope(void){ Scope* s=(Scope*)calloc(1,sizeof(Scope)); s->parent=current_scope; current_scope=s; }
static void st_pop_scope(void){ if(!current_scope) return; Scope* parent=current_scope->parent; st_free_list(current_scope->table); free(current_scope); current_scope=parent; }
static void st_reset(void){ while(current_scope) st_pop_scope(); }

static Sym* st_find_local(const char* id){ for(Sym* p=current_scope?current_scope->table:NULL;p;p=p->next) if(strcmp(p->name,id)==0) return p; return NULL; }
static Sym* st_find(const char* id){
  for(Scope* s=current_scope; s; s=s->parent) for(Sym* p=s->table; p; p=p->next) if(strcmp(p->name,id)==0) return p;
  return NULL;
}
static void st_insert(const char* id, Type t){
  if(!current_scope) st_push_scope();
  if(st_find_local(id)){ semf("[Semántico] Identificador redeclarado en el mismo ámbito: %s", id); return; }
  Sym* s=(Sym*)calloc(1,sizeof(Sym)); s->name=strdup(id); s->type=t; s->next=current_scope->table; current_scope->table=s;
}

/* -------- tabla de funciones -------- */
typedef struct Func { char* name; Type ret; int arity; Type* params; struct Func* next; } Func;
static Func* ftab = NULL;

static void f_reset(void){ Func* f=ftab; while(f){ Func* n=f->next; free(f->name); free(f->params); free(f); f=n; } ftab=NULL; }
static Func* f_find(const char* id){ for(Func* f=ftab; f; f=f->next) if(strcmp(f->name,id)==0) return f; return NULL; }
static void f_insert(const char* id, Type r, Type* ps, int n){
  if(f_find(id)){ semf("[Semántico] Función redeclarada: %s", id); return; }
  Func* f=(Func*)calloc(1,sizeof(Func));
  f->name=strdup(id); f->ret=r; f->arity=n;
  if(n>0){ f->params=(Type*)malloc(sizeof(Type)*n); memcpy(f->params, ps, sizeof(Type)*n); }
  f->next=ftab; ftab=f;
}

/* reservadas de seguridad */
static int is_reserved(const char* id){
  const char* R[]={"int","float","boolean","char","String","true","false","null",
                   "public","static","void","System","out","println",
                   "if","else","while","for","switch","case","default","break","continue","return",
                   "Integer","parseInt","Float","parseFloat","Double","parseDouble","valueOf","String","join","valueOf"};
  for(size_t i=0;i<sizeof(R)/sizeof(R[0]);++i) if(strcmp(id,R[i])==0) return 1;
  return 0;
}

/* helpers tipos */
static int is_numeric(Type t){ return t==TY_INT || t==TY_FLOAT; }
static Type promote_num(Type a, Type b){ return (a==TY_FLOAT||b==TY_FLOAT)? TY_FLOAT: TY_INT; }
static const char* tname(Type t){
  switch(t){
    case TY_INT: return "int"; case TY_FLOAT: return "float"; case TY_BOOL: return "boolean";
    case TY_CHAR: return "char"; case TY_STRING: return "String"; case TY_VOID: return "void";
    case TY_NULL: return "null"; case TY_ERROR: return "<error>"; default: return "<undef>";
  }
}
static int compatible_assign(Type dst, Type src){
  if(dst==TY_ERROR || src==TY_ERROR) return 1;
  if(src==TY_NULL) return is_reference(dst);
  if(dst==src) return 1;
  if(dst==TY_FLOAT && src==TY_INT) return 1;
  return 0;
}

/* === utilidades para validación de literales de parseo === */
static char* unquote(const char* lit){
  size_t n=strlen(lit);
  if(n>=2 && lit[0]=='"' && lit[n-1]=='"'){
    char* out=(char*)malloc(n-1);
    memcpy(out, lit+1, n-2);
    out[n-2]='\0';
    return out;
  }
  return strdup(lit);
}

/* VALIDACIÓN INDEPENDIENTE DEL LOCALE (punto como separador) */
static int is_float_string(const char* s){
  if(!s||!*s) return 0;
  const char* p = s;
  if(*p=='+' || *p=='-') p++;
  int digits_before = 0; while(*p && isdigit((unsigned char)*p)){ p++; digits_before++; }
  int has_dot = 0, digits_after = 0;
  if(*p=='.'){ has_dot = 1; p++; while(*p && isdigit((unsigned char)*p)){ p++; digits_after++; } }
  int has_digits = (digits_before + digits_after) > 0;
  if(*p=='e' || *p=='E'){ p++; if(*p=='+' || *p=='-') p++; int expd=0; while(*p && isdigit((unsigned char)*p)){ p++; expd++; } if(expd==0) return 0; }
  return has_digits && *p=='\0';
}

static int is_int_string(const char* s){
  if(!s||!*s) return 0;
  const char* p=s;
  if(*p=='+'||*p=='-') p++;
  int hasd=0; for(;*p;p++){ if(!isdigit((unsigned char)*p)) return 0; else hasd=1; }
  return hasd;
}

/* interfaz con flex/bison */
void yyerror(const char* s);
int yylex(void);

/* ---- contexto para break/continue y funciones ---- */
static int loop_depth = 0;
static int switch_depth = 0;
static int func_depth = 0;
static Type current_func_ret = TY_UNDEF;

/* expuestos para GUI */
void parser_reset(void){ st_reset(); f_reset(); log_reset(); semantic_errors = 0; loop_depth = 0; switch_depth = 0; func_depth = 0; current_func_ret = TY_UNDEF; }
const char* parser_get_log(void){ return LOG_BUF; }

/* vars auxiliares */
static Type current_decl_type = TY_UNDEF;
static char* for_tmp_id = NULL;
static Type  for_tmp_type = TY_UNDEF;
static Type fn_params[64];
static int  fn_arity = 0;

/* lista de tipos para args de llamada / join */
typedef struct TList { Type t; struct TList* next; } TList;
static TList* tlist_new(Type t, TList* next){ TList* n=(TList*)calloc(1,sizeof(TList)); n->t=t; n->next=next; return n; }
static TList* tlist_append(TList* a, Type t){ if(!a) return tlist_new(t,NULL); TList* p=a; while(p->next) p=p->next; p->next=tlist_new(t,NULL); return a; }
static int tlist_len(TList* a){ int k=0; for(TList* p=a;p;p=p->next) k++; return k; }
static void tlist_free(TList* a){ while(a){ TList* n=a->next; free(a); a=n; } }

%}
%define parse.error verbose

%union {
  int    ival;
  float  fval;
  char   cval;
  char*  sval;
  int    typ;     /* Type */
  struct TList* tlist;
}

/* tokens base */
%token T_PUBLIC T_STATIC T_VOID T_ID
%token T_INT T_FLOAT T_BOOLEAN T_CHAR T_STRING
%token T_TRUE T_FALSE T_NULL
%token T_INT_LIT T_FLOAT_LIT T_CHAR_LIT T_STRING_LIT
%token T_PRINTLN
%token T_IF T_ELSE
%token T_WHILE
%token T_FOR
%token T_SWITCH T_CASE T_DEFAULT T_BREAK T_CONTINUE
%token T_RETURN
%token T_INC T_DEC
%token T_PARSE_INT T_PARSE_FLOAT T_PARSE_DOUBLE
%token T_VALUEOF
%token T_JOIN   /* String.join / join */

/* operadores compuestos */
%token T_EQ T_NEQ T_GTE T_LTE
%token T_AND T_OR T_NOT
%token T_ADDEQ T_SUBEQ T_MULEQ T_DIVEQ T_MODEQ
%token T_ANDEQ T_OREQ T_XOREQ T_SHREQ T_SHLEQ

/* Tipos de yylval por token / no-tokens */
%type <sval> T_ID T_STRING_LIT
%type <ival> T_INT_LIT
%type <fval> T_FLOAT_LIT
%type <cval> T_CHAR_LIT
%type <typ>  tipo expresion
%type <typ>  for_init_notype opt_cond opt_post
%type <typ>  expr_or_assign assignment_expr
%type <typ>  case_const
%type <ival> params_opt param_list
%type <typ>  ret_type
%type <tlist> arglist_opt arglist
%type <typ>  func_call
%type <tlist> join_elems

%start programa

/* Precedencias y dangling else */
%left T_OR
%left T_AND
%left T_EQ T_NEQ
%left '>' '<' T_GTE T_LTE
%left '+' '-'
%left '*' '/' '%'
%right T_NOT
%nonassoc UMINUS
%nonassoc LOWER_THAN_ELSE
%nonassoc T_ELSE
%%

/* ===== Top-level ===== */
programa
  : lista_funciones { if(semantic_errors==0){ log_reset(); log_append("OK: parse exitoso"); } }
  ;

lista_funciones
  : lista_funciones func_decl
  | func_decl
  ;

opt_pubstat : /* vacío */ | T_PUBLIC T_STATIC ;

ret_type
  : tipo        { $$ = (Type)$1; }
  | T_VOID      { $$ = TY_VOID; }
  ;

params_opt
  : /* vacío */        { fn_arity = 0; $$ = 0; }
  | param_list         { $$ = $1; }
  ;

param_list
  : tipo T_ID
      { if(is_reserved($2)) semf("[Semántico] Identificador inválido como parámetro: '%s'", $2);
        else { st_insert($2,(Type)$1); if(fn_arity<64) fn_params[fn_arity++] = (Type)$1; }
        $$ = 1;
      }
  | param_list ',' tipo T_ID
      { if(is_reserved($4)) semf("[Semántico] Identificador inválido como parámetro: '%s'", $4);
        else { st_insert($4,(Type)$3); if(fn_arity<64) fn_params[fn_arity++] = (Type)$3; }
        $$ = $1 + 1;
      }
  ;

func_decl
  : opt_pubstat ret_type T_ID '('
      { st_push_scope(); current_func_ret = (Type)$2; func_depth++; fn_arity = 0; }
      params_opt ')'
      { f_insert($3, current_func_ret, fn_params, fn_arity); loop_depth=0; switch_depth=0; }
      '{' lista_sentencias '}'
      { st_pop_scope(); func_depth--; current_func_ret = TY_UNDEF; }
  ;

/* ===== Sentencias ===== */
lista_sentencias
  : /* vacío */
  | lista_sentencias sentencia
  ;

sentencia
  : declaracion
  | asignacion ';'
  | impresion ';'
  | func_call ';'
  | bloque
  | if_stmt
  | while_stmt
  | for_stmt
  | switch_stmt
  | break_stmt
  | continue_stmt
  | return_stmt
  ;

bloque
  : '{' { st_push_scope(); } lista_sentencias '}' { st_pop_scope(); }
  ;

/* ===== If / Else ===== */
if_stmt
  : T_IF '(' expresion ')' sentencia %prec LOWER_THAN_ELSE
      { if($3!=TY_BOOL) semf("[Semántico] La condición del if debe ser boolean (recibido %s)", tname($3)); }
  | T_IF '(' expresion ')' sentencia T_ELSE sentencia
      { if($3!=TY_BOOL) semf("[Semántico] La condición del if debe ser boolean (recibido %s)", tname($3)); }
  ;

/* ===== While ===== */
enter_loop : /* vacío */ { loop_depth++; } ;

while_stmt
  : T_WHILE '(' expresion ')' enter_loop sentencia
      { if($3!=TY_BOOL) semf("[Semántico] La condición del while debe ser boolean (recibido %s)", tname($3)); loop_depth--; if(loop_depth<0) loop_depth=0; }
  ;

/* ===== For / Foreach ===== */
mkscope : /* vacío */ { st_push_scope(); } ;

for_each_intro
  : T_FOR '(' mkscope tipo T_ID ':' expresion ')'
      {
        Type varT  = (Type)$4;
        Type iterT = (Type)$7;
        if(is_reserved($5)) semf("[Semántico] Identificador inválido: '%s' es palabra reservada", $5);
        else st_insert($5, varT);

        if(iterT != TY_ERROR){
          if(iterT != TY_STRING)
            semf("[Semántico] foreach: expresión no iterable (solo String)");
          else if(varT != TY_CHAR)
            semf("[Semántico] foreach sobre String requiere variable de tipo char, no %s", tname(varT));
        }
      }
  ;

for_stmt
  : for_each_intro enter_loop sentencia
      { st_pop_scope(); loop_depth--; if(loop_depth<0) loop_depth=0; }

  | T_FOR '(' mkscope
      tipo T_ID { for_tmp_type=(Type)$4; for_tmp_id=$5; }
      for_decl_tail ';' opt_cond ';' opt_post ')' enter_loop sentencia
      {
        if($9!=TY_UNDEF && $9!=TY_BOOL)
          semf("[Semántico] La condición del for debe ser boolean (recibido %s)", tname($9));
        st_pop_scope(); loop_depth--; if(loop_depth<0) loop_depth=0;
      }

  | T_FOR '(' mkscope for_init_notype ';' opt_cond ';' opt_post ')' enter_loop sentencia
      {
        if($6!=TY_UNDEF && $6!=TY_BOOL)
          semf("[Semántico] La condición del for debe ser boolean (recibido %s)", tname($6));
        st_pop_scope(); loop_depth--; if(loop_depth<0) loop_depth=0;
      }
  ;

for_decl_tail
  : first_decl_rest more_decls
  ;

first_decl_rest
  : '=' expresion
      {
        if(is_reserved(for_tmp_id)) semf("[Semántico] Identificador inválido: '%s' es palabra reservada", for_tmp_id);
        else {
          st_insert(for_tmp_id, for_tmp_type);
          if(!compatible_assign(for_tmp_type, (Type)$2))
            semf("[Semántico] No compatible: %s = %s", tname(for_tmp_type), tname((Type)$2));
        }
      }
  | /* vacío */
      {
        if(is_reserved(for_tmp_id)) semf("[Semántico] Identificador inválido: '%s' es palabra reservada", for_tmp_id);
        else st_insert(for_tmp_id, for_tmp_type);
      }
  ;

more_decls
  : /* vacío */
  | ',' { current_decl_type = for_tmp_type; } lista_declaradores
  ;

for_init_notype
  : /* vacío */            { $$ = TY_UNDEF; }
  | expr_or_assign         { $$ = $1; }
  ;

opt_cond
  : /* vacío */        { $$ = TY_UNDEF; }
  | expresion          { $$ = $1; }
  ;

opt_post
  : /* vacío */        { $$ = TY_UNDEF; }
  | expr_or_assign     { $$ = $1; }
  ;

/* ===== Asignación como EXPRESIÓN ===== */
expr_or_assign
  : expresion           { $$ = $1; }
  | assignment_expr     { $$ = TY_UNDEF; }
  ;

/* ===== Asignaciones (sentencia) ===== */
asignacion
  : assignment_expr     { }
  ;

assignment_expr
  : T_ID '=' expresion
    { Sym* s=st_find($1); if(!s) semf("[Semántico] Uso de variable no declarada: %s", $1);
      else if(!compatible_assign(s->type,(Type)$3)) semf("[Semántico] No compatible: %s = %s", tname(s->type), tname((Type)$3));
      $$ = (s ? s->type : TY_ERROR); }
  | T_ID T_INC
    { Sym* s=st_find($1); if(!s) semf("[Semántico] Uso de variable no declarada: %s", $1);
      else if(!is_numeric(s->type)) semf("[Semántico] '++' requiere tipo numérico");
      $$ = TY_UNDEF; }
  | T_ID T_DEC
    { Sym* s=st_find($1); if(!s) semf("[Semántico] Uso de variable no declarada: %s", $1);
      else if(!is_numeric(s->type)) semf("[Semántico] '--' requiere tipo numérico");
      $$ = TY_UNDEF; }
  | T_ID T_ADDEQ expresion
    { Sym* s=st_find($1); if(!s) semf("[Semántico] Uso de variable no declarada: %s", $1);
      else { Type res; if(s->type==TY_STRING||(Type)$3==TY_STRING) res=TY_STRING;
             else if(is_numeric(s->type)&&is_numeric((Type)$3)) res=promote_num(s->type,(Type)$3);
             else res=(SEMF("[Semántico] '+=' inválido entre %s y %s", tname(s->type), tname((Type)$3)),TY_ERROR);
             if(!compatible_assign(s->type,res)) semf("[Semántico] No compatible: %s += %s", tname(s->type), tname((Type)$3)); }
      $$ = TY_UNDEF; }
  | T_ID T_SUBEQ expresion
    { Sym* s=st_find($1); if(!s) semf("[Semántico] Uso de variable no declarada: %s", $1);
      else { Type res=(is_numeric(s->type)&&is_numeric((Type)$3))?promote_num(s->type,(Type)$3):(SEMF("[Semántico] '-=' requiere numéricos"),TY_ERROR);
             if(!compatible_assign(s->type,res)) semf("[Semántico] No compatible: %s -= %s", tname(s->type), tname((Type)$3)); }
      $$ = TY_UNDEF; }
  | T_ID T_MULEQ expresion
    { Sym* s=st_find($1); if(!s) semf("[Semántico] Uso de variable no declarada: %s", $1);
      else { Type res=(is_numeric(s->type)&&is_numeric((Type)$3))?promote_num(s->type,(Type)$3):(SEMF("[Semántico] '*=' requiere numéricos"),TY_ERROR);
             if(!compatible_assign(s->type,res)) semf("[Semántico] No compatible: %s *= %s", tname(s->type), tname((Type)$3)); }
      $$ = TY_UNDEF; }
  | T_ID T_DIVEQ expresion
    { Sym* s=st_find($1); if(!s) semf("[Semántico] Uso de variable no declarada: %s", $1);
      else { Type res=(is_numeric(s->type)&&is_numeric((Type)$3))?TY_FLOAT:(SEMF("[Semántico] '/=' requiere numéricos"),TY_ERROR);
             if(!compatible_assign(s->type,res)) semf("[Semántico] No compatible: %s /= %s", tname(s->type), tname((Type)$3)); }
      $$ = TY_UNDEF; }
  | T_ID T_MODEQ expresion
    { Sym* s=st_find($1); if(!s) semf("[Semántico] Uso de variable no declarada: %s", $1);
      else { Type res=(s->type==TY_INT&&(Type)$3==TY_INT)?TY_INT:(SEMF("[Semántico] '%%=' requiere int %%= int"),TY_ERROR);
             if(!compatible_assign(s->type,res)) semf("[Semántico] No compatible: %s %%= %s", tname(s->type), tname((Type)$3)); }
      $$ = TY_UNDEF; }
  | T_ID T_ANDEQ expresion
    { Sym* s=st_find($1); if(!s) semf("[Semántico] Uso de variable no declarada: %s", $1);
      else { Type res=(s->type==TY_INT&&(Type)$3==TY_INT)?TY_INT:(SEMF("[Semántico] '&=' requiere int &= int"),TY_ERROR);
             if(!compatible_assign(s->type,res)) semf("[Semántico] No compatible: %s &= %s", tname(s->type), tname((Type)$3)); }
      $$ = TY_UNDEF; }
  | T_ID T_OREQ expresion
    { Sym* s=st_find($1); if(!s) semf("[Semántico] Uso de variable no declarada: %s", $1);
      else { Type res=(s->type==TY_INT&&(Type)$3==TY_INT)?TY_INT:(SEMF("[Semántico] '|=' requiere int |= int"),TY_ERROR);
             if(!compatible_assign(s->type,res)) semf("[Semántico] No compatible: %s |= %s", tname(s->type), tname((Type)$3)); }
      $$ = TY_UNDEF; }
  | T_ID T_XOREQ expresion
    { Sym* s=st_find($1); if(!s) semf("[Semántico] Uso de variable no declarada: %s", $1);
      else { Type res=(s->type==TY_INT&&(Type)$3==TY_INT)?TY_INT:(SEMF("[Semántico] '^=' requiere int ^= int"),TY_ERROR);
             if(!compatible_assign(s->type,res)) semf("[Semántico] No compatible: %s ^= %s", tname(s->type), tname((Type)$3)); }
      $$ = TY_UNDEF; }
  | T_ID T_SHREQ expresion
    { Sym* s=st_find($1); if(!s) semf("[Semántico] Uso de variable no declarada: %s", $1);
      else { Type res=(s->type==TY_INT&&(Type)$3==TY_INT)?TY_INT:(SEMF("[Semántico] '>>=' requiere int >> = int"),TY_ERROR);
             if(!compatible_assign(s->type,res)) semf("[Semántico] No compatible: %s >>= %s", tname(s->type), tname((Type)$3)); }
      $$ = TY_UNDEF; }
  | T_ID T_SHLEQ expresion
    { Sym* s=st_find($1); if(!s) semf("[Semántico] Uso de variable no declarada: %s", $1);
      else { Type res=(s->type==TY_INT&&(Type)$3==TY_INT)?TY_INT:(SEMF("[Semántico] '<<=' requiere int <<= int"),TY_ERROR);
             if(!compatible_assign(s->type,res)) semf("[Semántico] No compatible: %s <<= %s", tname(s->type), tname((Type)$3)); }
      $$ = TY_UNDEF; }
  ;

/* ===== Declaraciones ===== */
declaracion
  : tipo { current_decl_type = (Type)$1; } lista_declaradores ';'
  ;

lista_declaradores
  : declarador
  | lista_declaradores ',' declarador
  ;

declarador
  : T_ID
      { if(is_reserved($1)) semf("[Semántico] Identificador inválido: '%s' es palabra reservada", $1);
        else st_insert($1, current_decl_type); }
  | T_ID '=' expresion
      { if(is_reserved($1)) semf("[Semántico] Identificador inválido: '%s' es palabra reservada", $1);
        else { st_insert($1, current_decl_type);
               if(!compatible_assign(current_decl_type, (Type)$3))
                 semf("[Semántico] No compatible: %s = %s", tname(current_decl_type), tname((Type)$3)); } }
  ;

tipo
  : T_INT      { $$ = TY_INT; }
  | T_FLOAT    { $$ = TY_FLOAT; }
  | T_BOOLEAN  { $$ = TY_BOOL; }
  | T_CHAR     { $$ = TY_CHAR; }
  | T_STRING   { $$ = TY_STRING; }
  ;

/* ===== Switch / Case ===== */
enter_switch : /* vacío */ { switch_depth++; } ;

switch_stmt
  : T_SWITCH '(' expresion ')' '{'
      {
        Type t = $3;
        if(t!=TY_ERROR && t!=TY_INT && t!=TY_CHAR && t!=TY_STRING)
          semf("[Semántico] switch: tipo no soportado %s (solo int, char, String)", tname(t));
      }
      enter_switch case_list opt_default
    '}' { switch_depth--; if(switch_depth<0) switch_depth=0; }
  ;

case_list
  : /* vacío */
  | case_list case_item
  ;

case_item
  : T_CASE case_const ':' lista_sentencias
  ;

opt_default
  : /* vacío */
  | T_DEFAULT ':' lista_sentencias
  ;

case_const
  : T_INT_LIT     { $$ = TY_INT; }
  | T_CHAR_LIT    { $$ = TY_CHAR; }
  | T_STRING_LIT  { $$ = TY_STRING; }
  ;

/* ===== break / continue / return ===== */
break_stmt
  : T_BREAK ';'
      { if(loop_depth + switch_depth <= 0) semf("[Semántico] 'break' fuera de bucle o switch"); }
  ;

continue_stmt
  : T_CONTINUE ';'
      { if(loop_depth <= 0) semf("[Semántico] 'continue' fuera de bucle"); }
  ;

return_stmt
  : T_RETURN ';'
      { if(func_depth<=0) semf("[Semántico] 'return' fuera de función");
        else if(current_func_ret != TY_VOID) semf("[Semántico] 'return;' en función no-void (%s)", tname(current_func_ret));
      }
  | T_RETURN expresion ';'
      { if(func_depth<=0) semf("[Semántico] 'return' fuera de función");
        else if(current_func_ret == TY_VOID) semf("[Semántico] 'return expr;' en función void");
        else if(!compatible_assign(current_func_ret, (Type)$2))
          semf("[Semántico] return incompatible: se esperaba %s y se obtuvo %s", tname(current_func_ret), tname((Type)$2));
      }
  ;

/* ===== Print ===== */
impresion
  : T_PRINTLN '(' expresion ')'
      { if ($3 == TY_ERROR) semf("[Semántico] println recibe expresión inválida"); }
  ;

/* ===== Llamadas a función ===== */
arglist_opt : /* vacío */ { $$ = NULL; } | arglist { $$ = $1; } ;
arglist
  : expresion                    { $$ = tlist_new((Type)$1, NULL); }
  | arglist ',' expresion        { $$ = tlist_append($1,(Type)$3); }
  ;

func_call
  : T_ID '(' arglist_opt ')'
      {
        Func* f = f_find($1);
        if(!f){ semf("[Semántico] Llamada a función no declarada: %s", $1); $$ = TY_ERROR; }
        else {
          int n = tlist_len($3);
          if(n != f->arity) semf("[Semántico] Número de argumentos a %s: se esperaban %d y se pasaron %d", f->name, f->arity, n);
          else {
            TList* p=$3; for(int i=0;i<n;i++,p=p->next){
              if(!compatible_assign(f->params[i], p->t))
                semf("[Semántico] Arg %d de %s: se esperaba %s y se obtuvo %s", i+1, f->name, tname(f->params[i]), tname(p->t));
            }
          }
          $$ = f->ret;
        }
        tlist_free($3);
      }
  ;

/* ===== Expresiones ===== */
join_elems
  : expresion                    { $$ = tlist_new((Type)$1, NULL); }
  | join_elems ',' expresion     { $$ = tlist_append($1,(Type)$3); }
  ;

expresion
  : func_call                { $$ = $1; }

  /* ---- String.join(delim, elem1, elem2, ...) → String ---- */
  | T_JOIN '(' expresion ',' join_elems ')'
      {
        int ok = 1;
        if($3 != TY_STRING){ semf("[Semántico] String.join: el delimitador debe ser String"); ok = 0; }
        TList* p = $5;
        while(p){
          if(p->t != TY_STRING && p->t != TY_ERROR){
            semf("[Semántico] String.join: todos los elementos deben ser String (encontrado %s)", tname(p->t));
            ok = 0;
          }
          p = p->next;
        }
        $$ = ok ? TY_STRING : TY_ERROR;
        tlist_free($5);
      }

  /* ---- String.valueOf(any) → String ---- */
  | T_VALUEOF '(' expresion ')'
      { $$ = ($3==TY_ERROR ? TY_ERROR : TY_STRING); }

  /* ---- parseInt ---- */
  | T_PARSE_INT '(' T_STRING_LIT ')'
      { char* s=unquote($3);
        if(is_int_string(s)) $$ = TY_INT;
        else { semf("[Semántico] Integer.parseInt: cadena no numérica"); $$ = TY_ERROR; }
        free(s);
      }
  | T_PARSE_INT '(' expresion ')'
      { if($3 == TY_STRING) $$ = TY_INT;
        else { semf("[Semántico] Integer.parseInt requiere String"); $$ = TY_ERROR; } }

  /* ---- parseFloat / parseDouble → float ---- */
  | T_PARSE_FLOAT '(' T_STRING_LIT ')'
      { char* s=unquote($3);
        if(is_float_string(s)) $$ = TY_FLOAT;
        else { semf("[Semántico] Float.parseFloat: formato no válido"); $$ = TY_ERROR; }
        free(s);
      }
  | T_PARSE_FLOAT '(' expresion ')'
      { if($3 == TY_STRING) $$ = TY_FLOAT;
        else { semf("[Semántico] Float.parseFloat requiere String"); $$ = TY_ERROR; } }

  | T_PARSE_DOUBLE '(' T_STRING_LIT ')'
      { char* s=unquote($3);
        if(is_float_string(s)) $$ = TY_FLOAT; /* mapeamos a float */
        else { semf("[Semántico] Double.parseDouble: formato no válido"); $$ = TY_ERROR; }
        free(s);
      }
  | T_PARSE_DOUBLE '(' expresion ')'
      { if($3 == TY_STRING) $$ = TY_FLOAT;
        else { semf("[Semántico] Double.parseDouble requiere String"); $$ = TY_ERROR; } }

  | T_NULL                   { $$ = TY_NULL; }
  | T_INT_LIT        { $$ = TY_INT; }
  | T_FLOAT_LIT      { $$ = TY_FLOAT; }
  | T_CHAR_LIT       { $$ = TY_CHAR; }
  | T_STRING_LIT     { $$ = TY_STRING; }
  | T_TRUE           { $$ = TY_BOOL; }
  | T_FALSE          { $$ = TY_BOOL; }
  | T_ID             { Sym* s=st_find($1); if(!s){ semf("[Semántico] Uso de variable no declarada: %s", $1); $$ = TY_ERROR; } else $$ = s->type; }
  | '(' expresion ')' { $$ = $2; }
  | '-' expresion %prec UMINUS { $$ = (is_numeric($2)? $2 : (SEMF("'-' requiere numérico"), TY_ERROR)); }
  | T_NOT expresion            { $$ = ($2==TY_BOOL? TY_BOOL : (SEMF("'!' requiere boolean"), TY_ERROR)); }
  | expresion '*' expresion    { $$ = (is_numeric($1)&&is_numeric($3))? promote_num($1,$3):(SEMF("'*' requiere numéricos"),TY_ERROR); }
  | expresion '/' expresion    { $$ = (is_numeric($1)&&is_numeric($3))? TY_FLOAT:(SEMF("'/' requiere numéricos"),TY_ERROR); }
  | expresion '%' expresion    { $$ = ($1==TY_INT && $3==TY_INT)? TY_INT:(SEMF("'%%' requiere int %% int"),TY_ERROR); }
  | expresion '+' expresion
      { if(is_numeric($1)&&is_numeric($3)) $$ = promote_num($1,$3);
        else if($1==TY_STRING||$3==TY_STRING) $$ = TY_STRING;
        else { $$ = (SEMF("'+' inválido entre %s y %s", tname($1), tname($3)), TY_ERROR); } }
  | expresion '-' expresion    { $$ = (is_numeric($1)&&is_numeric($3))? promote_num($1,$3):(SEMF("'-' requiere numéricos"),TY_ERROR); }
  | expresion '<' expresion    { $$ = (is_numeric($1)&&is_numeric($3))? TY_BOOL:(SEMF("'<' requiere numéricos"),TY_ERROR); }
  | expresion '>' expresion    { $$ = (is_numeric($1)&&is_numeric($3))? TY_BOOL:(SEMF("'>' requiere numéricos"),TY_ERROR); }
  | expresion T_LTE expresion  { $$ = (is_numeric($1)&&is_numeric($3))? TY_BOOL:(SEMF("'<=' requiere numéricos"),TY_ERROR); }
  | expresion T_GTE expresion  { $$ = (is_numeric($1)&&is_numeric($3))? TY_BOOL:(SEMF("'>=' requiere numéricos"),TY_ERROR); }

  /* == y != con null (sólo con String/null) */
  | expresion T_EQ expresion
      {
        if($1==TY_ERROR||$3==TY_ERROR) { $$ = TY_ERROR; }
        else if($1==TY_NULL || $3==TY_NULL){
          Type other = ($1==TY_NULL)? $3 : $1;
          if(!(other==TY_STRING || other==TY_NULL)){
            semf("[Semántico] '==' con null solo válido contra String/null");
            $$ = TY_ERROR;
          } else $$ = TY_BOOL;
        } else $$ = TY_BOOL;
      }
  | expresion T_NEQ expresion
      {
        if($1==TY_ERROR||$3==TY_ERROR) { $$ = TY_ERROR; }
        else if($1==TY_NULL || $3==TY_NULL){
          Type other = ($1==TY_NULL)? $3 : $1;
          if(!(other==TY_STRING || other==TY_NULL)){
            semf("[Semántico] '!=' con null solo válido contra String/null");
            $$ = TY_ERROR;
          } else $$ = TY_BOOL;
        } else $$ = TY_BOOL;
      }

  | expresion T_AND expresion  { $$ = ($1==TY_BOOL&&$3==TY_BOOL)? TY_BOOL:(SEMF("'&&' requiere boolean"),TY_ERROR); }
  | expresion T_OR  expresion  { $$ = ($1==TY_BOOL&&$3==TY_BOOL)? TY_BOOL:(SEMF("'||' requiere boolean"),TY_ERROR); }
  ;

%%
void yyerror(const char* s){ semf("[Syntax] %s", s); }