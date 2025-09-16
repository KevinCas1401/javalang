%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <ctype.h>
/* --- FWD del reset del lexer --- */
void lexer_reset_position(void);


/* -------- logging -------- */
static int semantic_errors = 0;
static char LOG_BUF[16384];
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

/* === FWD DECLARATION para el reporte de errores === */
static void errrep_addf(const char* fmt, ...);

static int semf(const char* fmt, ...){
  char __tmp[512];
  va_list ap; va_start(ap, fmt);
  vsnprintf(__tmp, sizeof(__tmp), fmt, ap);
  va_end(ap);
  log_append(__tmp);
  /* NUEVO: guardar en el listado de errores con (ámbito, línea, columna) */
  errrep_addf("%s", __tmp);
  semantic_errors++;
  return 0;
}

#define LOGF(...)  do{ char __tmp[512]; snprintf(__tmp,sizeof(__tmp), __VA_ARGS__); log_append(__tmp);}while(0)
#define SEMF(...)  (semf(__VA_ARGS__), 0)

/* ==================== REPORTE DE ERRORES ==================== */
int g_last_line = 1;
int g_last_col  = 1;

typedef struct ErrRep {
  char* descripcion;
  char* ambito;
  int   linea;
  int   columna;
  struct ErrRep* next;
} ErrRep;

static ErrRep* ERR_HEAD = NULL;
static ErrRep* ERR_TAIL = NULL;

static void errrep_reset(void){
  ErrRep* p = ERR_HEAD;
  while(p){
    ErrRep* n = p->next;
    free(p->descripcion);
    free(p->ambito);
    free(p);
    p = n;
  }
  ERR_HEAD = ERR_TAIL = NULL;
}






/* -------- Tipos -------- */
typedef enum { TY_UNDEF=0, TY_INT, TY_FLOAT, TY_DOUBLE, TY_BOOL, TY_CHAR, TY_STRING, TY_VOID, TY_NULL, TY_ERROR } Type;
static int is_reference(Type t){ return t==TY_STRING; }

/* -------- Salida de ejecución -------- */
static char OUT_BUF[16384];
static size_t OUT_POS = 0;
static void out_reset(void){ OUT_POS = 0; OUT_BUF[0] = '\0'; }
static void out_append(const char* s){
  if(!s) return;
  size_t n=strlen(s);
  if(OUT_POS+n+2 < sizeof(OUT_BUF)){
    memcpy(OUT_BUF+OUT_POS, s, n);
    OUT_POS += n;
    OUT_BUF[OUT_POS++] = '\n';
    OUT_BUF[OUT_POS] = '\0';
  }
}

/* -------- Valores -------- */
typedef struct {
  Type type;
  union { int ival; float fval; char cval; char* sval; int bval; } u;
} Value;

static Value V_int(int x){ Value v; v.type=TY_INT; v.u.ival=x; return v; }
static Value V_float(float x){ Value v; v.type=TY_FLOAT; v.u.fval=x; return v; }
static Value V_bool(int x){ Value v; v.type=TY_BOOL; v.u.bval=(x?1:0); return v; }
static Value V_char(char c){ Value v; v.type=TY_CHAR; v.u.cval=c; return v; }
static Value V_string(const char* s){ Value v; v.type=TY_STRING; v.u.sval = s?strdup(s):strdup(""); return v; }
static Value V_null(void){ Value v; v.type=TY_NULL; return v; }
static Value V_void(void){ Value v; v.type=TY_VOID; return v; }
static void value_free(Value* v){ if(v && v->type==TY_STRING && v->u.sval){ free(v->u.sval); v->u.sval=NULL; } }

/* ==================== TS con Ámbitos + Posiciones ==================== */
typedef struct Sym {
  char* name;
  Type type;
  Value val;
  char* kind;          /* "Variable" | "Función" */
  char* scope_name;    /* "Global", "main", "Block#N", etc. */
  int   line, col;     /* posición de declaración */
  struct Sym* next;
} Sym;

typedef struct Scope {
  Sym* table;
  struct Scope* parent;
  char* scope_name;
} Scope;


static Scope* root_scope = NULL;
static int block_counter = 0;
static Scope* current_scope = NULL;

/* agrega error usando los últimos (line,col) del lexer y el ámbito actual */
static void errrep_addf(const char* fmt, ...){
  char msg[512];
  va_list ap; va_start(ap, fmt);
  vsnprintf(msg, sizeof(msg), fmt, ap);
  va_end(ap);

  ErrRep* e = (ErrRep*)calloc(1,sizeof(ErrRep));
  e->descripcion = strdup(msg);
  e->ambito = strdup(current_scope && current_scope->scope_name ? current_scope->scope_name : "Global");
  e->linea = g_last_line;
  e->columna = g_last_col;

  if(!ERR_HEAD){ ERR_HEAD = ERR_TAIL = e; }
  else { ERR_TAIL->next = e; ERR_TAIL = e; }
}

/* escribe el reporte en TXT con encabezados */
static const char* parser_write_errors_txt(const char* path){
  static char LAST_PATH[256];
  if(!path || !*path) path = "Errores.txt";
  FILE* f = fopen(path, "w");
  if(!f) return NULL;

  fprintf(f, "No.\tDescripción\tÁmbito\tLinea\tColumna\n");
  int i = 1;
  for(ErrRep* p = ERR_HEAD; p; p = p->next, ++i){
    fprintf(f, "%d\t%s\t%s\t%d\t%d\n",
            i,
            p->descripcion ? p->descripcion : "",
            p->ambito ? p->ambito : "Global",
            p->linea,
            p->columna);
  }
  fclose(f);
  snprintf(LAST_PATH, sizeof(LAST_PATH), "%s", path);
  return LAST_PATH;
}



/* ===== Copia de símbolos para reporte ===== */
typedef struct SymRep {
  char* name;
  char* kind;        /* "Variable" | "Función" */
  char* scope_name;  /* "Global", "main", "Block#N", etc. */
  Type  type;
  int   line, col;
  struct SymRep* next;
} SymRep;

static SymRep* SYMREP_HEAD = NULL;
static SymRep* SYMREP_TAIL = NULL;

static void symrep_reset(void){
  SymRep* p = SYMREP_HEAD;
  while(p){
    SymRep* n = p->next;
    free(p->name); free(p->kind); free(p->scope_name);
    free(p);
    p = n;
  }
  SYMREP_HEAD = SYMREP_TAIL = NULL;
}

static void symrep_add_from(const Sym* s){
  if(!s) return;
  SymRep* r = (SymRep*)calloc(1,sizeof(SymRep));
  r->name = strdup(s->name);
  r->kind = strdup(s->kind ? s->kind : "Variable");
  r->scope_name = strdup(s->scope_name ? s->scope_name : "Global");
  r->type = s->type;
  r->line = s->line; r->col = s->col;
  if(!SYMREP_HEAD){ SYMREP_HEAD = SYMREP_TAIL = r; }
  else { SYMREP_TAIL->next = r; SYMREP_TAIL = r; }
}




static void st_free_list(Sym* p){
  while(p){
    Sym* n=p->next;
    free(p->name);
    free(p->kind);
    free(p->scope_name);
    value_free(&p->val);
    free(p);
    p=n;
  }
}

static void st_push_scope_named(const char* nm){
  Scope* s=(Scope*)calloc(1,sizeof(Scope));
  s->parent=current_scope;
  s->table=NULL;
  s->scope_name = nm ? strdup(nm) : strdup("Anon");
  current_scope=s;
  if(!root_scope) root_scope = s;
}
static void st_push_scope(void){
  char buf[32];
  snprintf(buf,sizeof(buf),"Block#%d", ++block_counter);
  st_push_scope_named(buf);
}
static void st_pop_scope(void){
  if(!current_scope) return;
  Scope* parent=current_scope->parent;
  st_free_list(current_scope->table);
  free(current_scope->scope_name);
  if(current_scope==root_scope) root_scope=NULL;
  free(current_scope);
  current_scope=parent;
}
static void st_reset(void){
  while(current_scope) st_pop_scope();
  block_counter = 0;
  st_push_scope_named("Global");
  root_scope = current_scope;
}

static Sym* st_find_local(const char* id){ for(Sym* p=current_scope?current_scope->table:NULL;p;p=p->next) if(strcmp(p->name,id)==0) return p; return NULL; }
static Sym* st_find(const char* id){
  for(Scope* s=current_scope; s; s=s->parent) for(Sym* p=s->table; p; p=p->next) if(strcmp(p->name,id)==0) return p;
  return NULL;
}

/* Inserta símbolo con posición */
static Sym* st_insert_sym_pos_here(const char* id, Type t, const char* kind, int line, int col){
  if(!current_scope) st_reset();
  if(st_find_local(id)){ semf("[Semántico] Identificador redeclarado en el mismo ámbito: %s", id); return NULL; }
  Sym* s=(Sym*)calloc(1,sizeof(Sym));
  s->name=strdup(id);
  s->type=t;
  s->kind=strdup(kind?kind:"Variable");
  s->scope_name = strdup(current_scope && current_scope->scope_name ? current_scope->scope_name : "Global");
  s->line=line; s->col=col;
  if(t==TY_INT) s->val=V_int(0);
  else if(t==TY_FLOAT) s->val=V_float(0.0f);
  else if(t==TY_DOUBLE) s->val=V_float(0.0f);
  else if(t==TY_BOOL) s->val=V_bool(0);
  else if(t==TY_CHAR) s->val=V_char('\0');
  else if(t==TY_STRING) s->val=V_string("");
  else s->val=V_void();
  s->next=current_scope->table; current_scope->table=s;

  if(line > 0 || col > 0) {
    symrep_add_from(s);
}
   return s;
}

/* Reporte TSV de símbolos (encabezado + filas) */
static void st_dump_scope(FILE* f, Scope* sc){
  if(!sc) return;
  for(Sym* p=sc->table; p; p=p->next){
    fprintf(f, "%s\t%s\t%s\t%s\t%d\t%d\n",
            p->name,
            p->kind ? p->kind : "Variable",
            (p->type==TY_INT?"int": p->type==TY_FLOAT?"float": p->type==TY_DOUBLE?"double": p->type==TY_BOOL?"boolean": p->type==TY_CHAR?"char": p->type==TY_STRING?"String": p->type==TY_VOID?"void": p->type==TY_NULL?"null":"<undef>"),
            p->scope_name ? p->scope_name : "Global",
            p->line, p->col);
  }
}
static void st_dump_all(FILE* f, Scope* sc){
  if(!sc) return;
  /* recorrer hasta raíz y luego de raíz a hojas para listar Global primero */
  Scope* stack[256]; int n=0;
  for(Scope* s=sc; s; s=s->parent){ stack[n++]=s; if(n==256) break; }
  for(int i=n-1;i>=0;i--) st_dump_scope(f, stack[i]);
}
const char* parser_symbols_report(void){
  static char SYM_BUF[65536];
  SYM_BUF[0]='\0';
  FILE* mem = fmemopen(SYM_BUF, sizeof(SYM_BUF), "w");
  if(mem){
    fprintf(mem, "ID\tTipo símbolo\tTipo dato\tÁmbito\tLínea\tColumna\n");
    st_dump_all(mem, current_scope ? current_scope : root_scope);
    fclose(mem);
  }
  return SYM_BUF;
}

/* ====== REPORTE: TABLA DE SÍMBOLOS A TXT (desde copia) ====== */
const char* parser_write_symbols_txt(const char* path){
  static char LAST_PATH[256];
  if(!path || !*path) path = "TablaSimbolos.txt";
  FILE* f = fopen(path, "w");
  if(!f) return NULL;

  fprintf(f, "ID\tTipo símbolo\tTipo dato\tÁmbito\tLínea\tColumna\n");

  /* 1) Primero Global (suele contener las funciones) */
  for(SymRep* p = SYMREP_HEAD; p; p = p->next){
    if(p->scope_name && strcmp(p->scope_name, "Global")==0){
      fprintf(f, "%s\t%s\t%s\t%s\t%d\t%d\n",
        p->name, p->kind,
        (p->type==TY_INT?"int": p->type==TY_FLOAT?"float": p->type==TY_DOUBLE?"double":
         p->type==TY_BOOL?"boolean": p->type==TY_CHAR?"char": p->type==TY_STRING?"String":
         p->type==TY_VOID?"void": p->type==TY_NULL?"null":"<undef>"),
        p->scope_name, p->line, p->col);
    }
  }

  /* 2) Luego el resto de ámbitos (funciones, blocks, etc.) */
  for(SymRep* p = SYMREP_HEAD; p; p = p->next){
    if(!(p->scope_name && strcmp(p->scope_name, "Global")==0)){
      fprintf(f, "%s\t%s\t%s\t%s\t%d\t%d\n",
        p->name, p->kind,
        (p->type==TY_INT?"int": p->type==TY_FLOAT?"float": p->type==TY_DOUBLE?"double":
         p->type==TY_BOOL?"boolean": p->type==TY_CHAR?"char": p->type==TY_STRING?"String":
         p->type==TY_VOID?"void": p->type==TY_NULL?"null":"<undef>"),
        p->scope_name ? p->scope_name : "Global", p->line, p->col);
    }
  }

  fclose(f);
  snprintf(LAST_PATH, sizeof(LAST_PATH), "%s", path);
  return LAST_PATH;
}







/* -------- AST adelante -------- */
typedef struct Stmt Stmt;
typedef struct Expr Expr;

/* -------- Funciones -------- */
typedef struct Func {
  char* name;
  Type  ret;
  int   arity;
  Type* params;
  char** pnames;
  Stmt* body;
  struct Func* next;
} Func;

static Func* ftab = NULL;

static void f_reset(void){
  Func* f=ftab; 
  while(f){ 
    Func* n=f->next; 
    free(f->name); 
    if(f->params) free(f->params);
    if(f->pnames){ for(int i=0;i<f->arity;i++) free(f->pnames[i]); free(f->pnames); }
    free(f); 
    f=n; 
  }
  ftab=NULL;
}
static Func* f_find(const char* id){ for(Func* f=ftab; f; f=f->next) if(strcmp(f->name,id)==0) return f; return NULL; }
static Func* f_insert_placeholder(const char* id, Type r){
  Func* ex=f_find(id);
  if(ex) return ex;
  Func* f=(Func*)calloc(1,sizeof(Func));
  f->name=strdup(id);
  f->ret=r;
  f->arity=0;
  f->params=NULL;
  f->pnames=NULL;
  f->body=NULL;
  f->next=ftab; ftab=f;
  return f;
}

/* reservadas */
static int is_reserved(const char* id){
  const char* R[]={"int","float","double","boolean","char","String","true","false","null",
                   "public","static","void","System","out","println",
                   "if","else","while","do","for","switch","case","default","break","continue","return",
                   "Integer","parseInt","Float","parseFloat","Double","parseDouble","valueOf","join"};
  for(size_t i=0;i<sizeof(R)/sizeof(R[0]);++i) if(strcmp(id,R[i])==0) return 1;
  return 0;
}

/* helpers tipos */
static int is_numeric(Type t){ return t==TY_INT || t==TY_FLOAT || t==TY_DOUBLE; }
static Type promote_num(Type a, Type b){
  if(a==TY_DOUBLE || b==TY_DOUBLE) return TY_DOUBLE;
  return (a==TY_FLOAT||b==TY_FLOAT)? TY_FLOAT: TY_INT;
}
static const char* tname(Type t){
  switch(t){
    case TY_INT: return "int"; case TY_FLOAT: return "float"; case TY_DOUBLE: return "double";
    case TY_BOOL: return "boolean"; case TY_CHAR: return "char"; case TY_STRING: return "String";
    case TY_VOID: return "void"; case TY_NULL: return "null"; case TY_ERROR: return "<error>";
    default: return "<undef>";
  }
}
static int compatible_assign(Type dst, Type src){
  if(dst==TY_ERROR || src==TY_ERROR) return 1;
  if(src==TY_NULL) return is_reference(dst);
  if(dst==src) return 1;
  if(dst==TY_FLOAT && src==TY_INT) return 1;
  if(dst==TY_DOUBLE && (src==TY_INT || src==TY_FLOAT || src==TY_DOUBLE)) return 1;
  return 0;
}

/* literales */
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
static int is_float_string(const char* s){
  if(!s||!*s) return 0; const char* p = s;
  if(*p=='+' || *p=='-') p++;
  int db=0; while(*p && isdigit((unsigned char)*p)){ p++; db++; }
  int da = 0;
  if(*p=='.'){ p++; while(*p && isdigit((unsigned char)*p)){ p++; da++; } }
  int has_digits = (db + da) > 0;
  if(*p=='e' || *p=='E'){ p++; if(*p=='+' || *p=='-') p++; int expd=0; while(*p && isdigit((unsigned char)*p)){ p++; expd++; } if(expd==0) return 0; }
  return has_digits && *p=='\0';
}
static int is_int_string(const char* s){
  if(!s||!*s) return 0; const char* p=s;
  if(*p=='+'||*p=='-') p++;
  int hasd=0; for(;*p;p++){ if(!isdigit((unsigned char)*p)) return 0; else hasd=1; }
  return hasd;
}

/* interfaz con flex/bison */
void yyerror(const char* s);
int yylex(void);

/* contexto */
static int loop_depth = 0;
static int switch_depth = 0;
static int func_depth = 0;
static Type current_func_ret = TY_UNDEF;

void parser_reset(void){
  /* NUEVO: reset de posición del lexer */
  lexer_reset_position();
  g_last_line = 1;
  g_last_col  = 1;

  symrep_reset();  /* ya lo tienes */
  st_reset();
  f_reset();
  log_reset();
  out_reset();
  errrep_reset();
  semantic_errors = 0;
  loop_depth = 0;
  switch_depth = 0;
  func_depth = 0;
  current_func_ret = TY_UNDEF;
}


const char* parser_get_log(void){ return LOG_BUF; }
const char* exec_get_output(void){ return OUT_BUF; }
int parser_get_error_count(void){ return semantic_errors; }
/* Reporte TS para GUI */
const char* parser_symbols_report(void);

/* vars auxiliares */
static Type current_decl_type = TY_UNDEF;
static Type fn_params[64];
static char* fn_param_names[64];
static int  fn_arity = 0;

/* ==================== AST ==================== */
typedef enum {
  EK_INT, EK_FLOAT, EK_CHAR, EK_STRING, EK_BOOL, EK_NULL, EK_ID,
  EK_UNOP, EK_BINOP, EK_CALL, EK_MCALL, EK_JOIN, EK_VALUEOF, EK_PARSEINT, EK_PARSEFLOAT, EK_PARSEDOB
} ExprKind;

/* op codes internos */
enum { OP_NOT=1001, OP_AND, OP_OR, OP_EQ, OP_NEQ, OP_LTE, OP_GTE };

typedef struct ArgList {
  struct Expr* e;
  struct ArgList* next;
} ArgList;

struct Expr {
  ExprKind kind;
  Type type;
  union {
    int ival; float fval; char cval; char* sval; char* id;
    struct { int op; struct Expr* e; } unop;
    struct { int op; struct Expr* l; struct Expr* r; } binop;
    struct { char* fname; ArgList* args; } call;
    struct { struct Expr* recv; char* mname; ArgList* args; } mcall;
    struct { struct Expr* delim; ArgList* elems; } join;
    struct { struct Expr* e; } one;
  } u;
};

typedef struct StmtList { struct Stmt* s; struct StmtList* next; } StmtList;

typedef struct CaseBlock {
  struct Expr* label;
  StmtList* body;
  struct CaseBlock* next;
} CaseBlock;

typedef enum {
  SK_BLOCK, SK_SEQ, SK_DECL, SK_ASSIGN, SK_PRINT, SK_EXPRSTMT,
  SK_IF, SK_WHILE, SK_DOWHILE, SK_FOR, SK_SWITCH, SK_RETURN, SK_BREAK, SK_CONTINUE
} StmtKind;

typedef struct Stmt {
  StmtKind kind;
  union {
    struct { StmtList* list; } block;
    struct { char* id; Type t; struct Expr* init; } decl;
    struct { char* id; int op; struct Expr* rhs; } assign;
    struct { struct Expr* e; } print;
    struct { struct Expr* cond; struct Stmt* thenS; struct Stmt* elseS; } sif;
    struct { struct Expr* cond; struct Stmt* body; } swhile;
    struct { struct Stmt* body; struct Expr* cond; } sdowhile;
    struct { struct Stmt* init; struct Expr* cond; struct Stmt* post; struct Stmt* body; } sfor;
    struct { struct Expr* e; } sret;
    struct { struct Expr* e; } exprstmt;
    struct { struct Expr* sw; CaseBlock* cases; StmtList* deflt; } sswitch;
  } u;
};

/* Helpers AST */
static Expr* E_new(ExprKind k){ Expr* e=(Expr*)calloc(1,sizeof(Expr)); e->kind=k; e->type=TY_UNDEF; return e; }
static Stmt* S_new(StmtKind k){ Stmt* s=(Stmt*)calloc(1,sizeof(Stmt)); s->kind=k; return s; }
static StmtList* SL_push(StmtList* a, Stmt* s){ StmtList* n=(StmtList*)calloc(1,sizeof(StmtList)); n->s=s; n->next=NULL; if(!a) return n; StmtList* p=a; while(p->next) p=p->next; p->next=n; return a; }
static ArgList* AL_push(ArgList* a, Expr* e){ ArgList* n=(ArgList*)calloc(1,sizeof(ArgList)); n->e=e; n->next=NULL; if(!a) return n; ArgList* p=a; while(p->next) p=p->next; p->next=n; return a; }
static CaseBlock* CL_push(CaseBlock* a, Expr* lab, StmtList* body){ CaseBlock* n=(CaseBlock*)calloc(1,sizeof(CaseBlock)); n->label=lab; n->body=body; n->next=NULL; if(!a) return n; CaseBlock* p=a; while(p->next) p=p->next; p->next=n; return a; }



/* ==================== REPORTE AST EN TXT ==================== */

const char* parser_write_ast_txt(const char* path);

/* ---- helpers de impresión ---- */
static void ast_indent(FILE* f, int lvl){
  for(int i=0;i<lvl;i++) fputs("  ", f); /* 2 espacios por nivel */
}
static void ast_puts(FILE* f, int lvl, const char* s){
  ast_indent(f, lvl);
  fputs("- ", f);
  fputs(s, f);
  fputc('\n', f);
}

/* ---- nombre sencillo de tipos ---- */
static const char* ast_tname(Type t){ return tname(t); }

/* ---- stringify Expr ---- */
static void ast_dump_expr(FILE* f, struct Expr* e, int lvl);

static void ast_dump_binop_name(int op, char* out, size_t n){
  const char* s = NULL;
  switch(op){
    case '+': s="+"; break; case '-': s="-"; break; case '*': s="*"; break; case '/': s="/"; break; case '%': s="%"; break;
    case '<': s="<"; break; case '>': s=">"; break;
    case OP_LTE: s="<="; break; case OP_GTE: s=">="; break;
    case OP_EQ: s="=="; break; case OP_NEQ: s="!="; break;
    case OP_AND: s="&&"; break; case OP_OR: s="||"; break;
    default: s="?"; break;
  }
  snprintf(out, n, "%s", s);
}

static void ast_dump_expr(FILE* f, struct Expr* e, int lvl){
  if(!e){ ast_puts(f,lvl,"<expr:null>"); return; }
  char buf[128];
  switch(e->kind){
    case EK_INT: snprintf(buf,sizeof(buf),"Int(%d)", e->u.ival); ast_puts(f,lvl,buf); break;
    case EK_FLOAT: snprintf(buf,sizeof(buf),"Float(%g)", e->u.fval); ast_puts(f,lvl,buf); break;
    case EK_CHAR: snprintf(buf,sizeof(buf),"Char('%c')", e->u.cval); ast_puts(f,lvl,buf); break;
    case EK_STRING: snprintf(buf,sizeof(buf),"String(\"%s\")", e->u.sval?e->u.sval:""); ast_puts(f,lvl,buf); break;
    case EK_BOOL: snprintf(buf,sizeof(buf),"Boolean(%s)", e->u.ival? "true":"false"); ast_puts(f,lvl,buf); break;
    case EK_NULL: ast_puts(f,lvl,"null"); break;
    case EK_ID: snprintf(buf,sizeof(buf),"Id(%s)", e->u.id); ast_puts(f,lvl,buf); break;
    case EK_UNOP:
      if(e->u.unop.op==OP_NOT) ast_puts(f,lvl,"UnOp(!)");
      else if(e->u.unop.op=='-') ast_puts(f,lvl,"UnOp(-)");
      else ast_puts(f,lvl,"UnOp(?)");
      ast_dump_expr(f, e->u.unop.e, lvl+1);
      break;
    case EK_BINOP: {
      char opn[8]; ast_dump_binop_name(e->u.binop.op, opn, sizeof(opn));
      snprintf(buf,sizeof(buf),"BinOp(%s)", opn); ast_puts(f,lvl,buf);
      ast_dump_expr(f, e->u.binop.l, lvl+1);
      ast_dump_expr(f, e->u.binop.r, lvl+1);
      break;
    }
    case EK_CALL:
      snprintf(buf,sizeof(buf),"Call(%s)", e->u.call.fname? e->u.call.fname:"<anon>"); ast_puts(f,lvl,buf);
      for(ArgList* a=e->u.call.args;a;a=a->next) ast_dump_expr(f, a->e, lvl+1);
      break;
    case EK_MCALL:
      snprintf(buf,sizeof(buf),"MethodCall(.%s)", e->u.mcall.mname? e->u.mcall.mname:"<?>"); ast_puts(f,lvl,buf);
      ast_dump_expr(f, e->u.mcall.recv, lvl+1);
      for(ArgList* a=e->u.mcall.args;a;a=a->next) ast_dump_expr(f, a->e, lvl+1);
      break;
    case EK_JOIN:
      ast_puts(f,lvl,"String.join");
      ast_puts(f,lvl+1,"Delimiter");
      ast_dump_expr(f, e->u.join.delim, lvl+2);
      ast_puts(f,lvl+1,"Elements");
      for(ArgList* a=e->u.join.elems;a;a=a->next) ast_dump_expr(f, a->e, lvl+2);
      break;
    case EK_VALUEOF:
      ast_puts(f,lvl,"String.valueOf");
      ast_dump_expr(f, e->u.one.e, lvl+1);
      break;
    case EK_PARSEINT: ast_puts(f,lvl,"Integer.parseInt"); ast_dump_expr(f, e->u.one.e, lvl+1); break;
    case EK_PARSEFLOAT: ast_puts(f,lvl,"Float.parseFloat"); ast_dump_expr(f, e->u.one.e, lvl+1); break;
    case EK_PARSEDOB: ast_puts(f,lvl,"Double.parseDouble"); ast_dump_expr(f, e->u.one.e, lvl+1); break;
    default: ast_puts(f,lvl,"<expr:?>"); break;
  }
}

/* ---- stringify Stmt ---- */
static void ast_dump_stmt(FILE* f, struct Stmt* s, int lvl);

static void ast_dump_stmt_list(FILE* f, struct StmtList* L, int lvl){
  for(StmtList* p=L;p;p=p->next) ast_dump_stmt(f, p->s, lvl);
}

static void ast_dump_cases(FILE* f, struct CaseBlock* c, int lvl){
  for(CaseBlock* p=c;p;p=p->next){
    ast_puts(f,lvl,"case");
    ast_dump_expr(f, p->label, lvl+1);
    ast_dump_stmt_list(f, p->body, lvl+1);
  }
}

static void ast_dump_stmt(FILE* f, struct Stmt* s, int lvl){
  if(!s){ ast_puts(f,lvl,"<stmt:null>"); return; }
  char buf[128];
  switch(s->kind){
    case SK_BLOCK:
      ast_puts(f,lvl,"Block");
      ast_dump_stmt_list(f, s->u.block.list, lvl+1);
      break;
    case SK_SEQ:
      ast_puts(f,lvl,"Seq");
      ast_dump_stmt_list(f, s->u.block.list, lvl+1);
      break;
    case SK_DECL:
      snprintf(buf,sizeof(buf),"Decl %s : %s", s->u.decl.id, ast_tname(s->u.decl.t));
      ast_puts(f,lvl,buf);
      if(s->u.decl.init){
        ast_puts(f,lvl+1,"Init");
        ast_dump_expr(f, s->u.decl.init, lvl+2);
      }
      break;
    case SK_ASSIGN:
      snprintf(buf,sizeof(buf),"Assign %s =", s->u.assign.id);
      ast_puts(f,lvl,buf);
      ast_dump_expr(f, s->u.assign.rhs, lvl+1);
      break;
    case SK_PRINT:
      ast_puts(f,lvl,"println");
      ast_dump_expr(f, s->u.print.e, lvl+1);
      break;
    case SK_EXPRSTMT:
      ast_puts(f,lvl,"ExprStmt");
      ast_dump_expr(f, s->u.exprstmt.e, lvl+1);
      break;
    case SK_IF:
      ast_puts(f,lvl,"if");
      ast_puts(f,lvl+1,"cond");
      ast_dump_expr(f, s->u.sif.cond, lvl+2);
      ast_puts(f,lvl+1,"then");
      ast_dump_stmt(f, s->u.sif.thenS, lvl+2);
      if(s->u.sif.elseS){
        ast_puts(f,lvl+1,"else");
        ast_dump_stmt(f, s->u.sif.elseS, lvl+2);
      }
      break;
    case SK_WHILE:
      ast_puts(f,lvl,"while");
      ast_puts(f,lvl+1,"cond");
      ast_dump_expr(f, s->u.swhile.cond, lvl+2);
      ast_puts(f,lvl+1,"body");
      ast_dump_stmt(f, s->u.swhile.body, lvl+2);
      break;
    case SK_DOWHILE:
      ast_puts(f,lvl,"do-while");
      ast_puts(f,lvl+1,"body");
      ast_dump_stmt(f, s->u.sdowhile.body, lvl+2);
      ast_puts(f,lvl+1,"cond");
      ast_dump_expr(f, s->u.sdowhile.cond, lvl+2);
      break;
    case SK_FOR:
      ast_puts(f,lvl,"for");
      if(s->u.sfor.init){ ast_puts(f,lvl+1,"init"); ast_dump_stmt(f, s->u.sfor.init, lvl+2); }
      ast_puts(f,lvl+1,"cond"); ast_dump_expr(f, s->u.sfor.cond, lvl+2);
      if(s->u.sfor.post){ ast_puts(f,lvl+1,"post"); ast_dump_stmt(f, s->u.sfor.post, lvl+2); }
      ast_puts(f,lvl+1,"body"); ast_dump_stmt(f, s->u.sfor.body, lvl+2);
      break;
    case SK_SWITCH:
      ast_puts(f,lvl,"switch");
      ast_puts(f,lvl+1,"expr");
      ast_dump_expr(f, s->u.sswitch.sw, lvl+2);
      ast_puts(f,lvl+1,"cases");
      ast_dump_cases(f, s->u.sswitch.cases, lvl+2);
      if(s->u.sswitch.deflt){
        ast_puts(f,lvl+1,"default");
        ast_dump_stmt_list(f, s->u.sswitch.deflt, lvl+2);
      }
      break;
    case SK_RETURN:
      ast_puts(f,lvl,"return");
      if(s->u.sret.e) ast_dump_expr(f, s->u.sret.e, lvl+1);
      break;
    case SK_BREAK: ast_puts(f,lvl,"break"); break;
    case SK_CONTINUE: ast_puts(f,lvl,"continue"); break;
    default: ast_puts(f,lvl,"<stmt:?>"); break;
  }
}

/* ---- volcamos todas las funciones registradas ---- */
static void ast_dump_functions(FILE* f){
  /* ftab está en lista enlazada; lo invertimos a vector para imprimir en orden lógico */
  int n=0; for(Func* p=ftab;p;p=p->next) n++;
  Func** arr = (Func**)calloc(n>0?n:1, sizeof(Func*));
  int i=0; for(Func* p=ftab;p;p=p->next) arr[i++]=p;
  for(int k=n-1;k>=0;k--){
    Func* fn = arr[k];
    if(!fn) continue;
    char hdr[128];
    snprintf(hdr,sizeof(hdr),"Función %s : %s", fn->name?fn->name:"<anon>", tname(fn->ret));
    ast_puts(f,0,hdr);
    if(fn->arity>0){
      ast_puts(f,1,"Parámetros");
      for(int j=0;j<fn->arity;j++){
        char l[128];
        snprintf(l,sizeof(l),"%s : %s", fn->pnames?fn->pnames[j]:"p", tname(fn->params?fn->params[j]:TY_UNDEF));
        ast_puts(f,2,l);
      }
    }
    ast_puts(f,1,"Cuerpo");
    ast_dump_stmt(f, fn->body, 2);
  }
  free(arr);
}

/* ---- API: escribe el AST a un archivo TXT ---- */
const char* parser_write_ast_txt(const char* path){
  static char LAST_PATH[256];
  if(!path || !*path) path = "AST.txt";
  FILE* f = fopen(path, "w");
  if(!f) return NULL;
  fputs("Árbol de Sintaxis Abstracta (AST)\n", f);
  fputs("=================================\n\n", f);
  ast_dump_functions(f);
  fclose(f);
  snprintf(LAST_PATH, sizeof(LAST_PATH), "%s", path);
  return LAST_PATH;
}

/* ==================== EJECUTOR ==================== */
typedef enum { ES_NONE=0, ES_RETURN, ES_BREAK, ES_CONTINUE } ExecSignal;
typedef struct { ExecSignal sig; Value ret; } ExecResult;

static char* str_cat(const char* a, const char* b){
  if(!a) a=""; if(!b) b="";
  size_t na=strlen(a), nb=strlen(b);
  char* r=(char*)malloc(na+nb+1);
  memcpy(r,a,na); memcpy(r+na,b,nb); r[na+nb]='\0'; return r;
}
static char* value_to_cstr(Value v){
  char buf[64];
  switch(v.type){
    case TY_INT:    snprintf(buf,sizeof(buf),"%d",v.u.ival); return strdup(buf);
    case TY_FLOAT:  snprintf(buf,sizeof(buf),"%g",v.u.fval); return strdup(buf);
    case TY_DOUBLE: snprintf(buf,sizeof(buf),"%g",(double)v.u.fval); return strdup(buf);
    case TY_BOOL:   return strdup(v.u.bval? "true":"false");
    case TY_CHAR:   { char s[2]={v.u.cval,0}; return strdup(s); }
    case TY_STRING: return strdup(v.u.sval?v.u.sval:"");
    case TY_NULL:   return strdup("null");
    default: return strdup("");
  }
}
static int values_equal(Value L, Value R){
  if(L.type==TY_STRING && R.type==TY_STRING){
    const char* a=L.u.sval?L.u.sval:""; const char* b=R.u.sval?R.u.sval:"";
    return strcmp(a,b)==0;
  }
  if((L.type==TY_CHAR && R.type==TY_CHAR)) return L.u.cval==R.u.cval;
  if((L.type==TY_BOOL && R.type==TY_BOOL)) return L.u.bval==R.u.bval;
  if(is_numeric(L.type)&&is_numeric(R.type)){
    float lv=(L.type==TY_FLOAT||L.type==TY_DOUBLE?L.u.fval:L.u.ival);
    float rv=(R.type==TY_FLOAT||R.type==TY_DOUBLE?R.u.fval:R.u.ival);
    return lv==rv;
  }
  return 0;
}

static Value eval_expr(Expr* e); /* fwd */
static ExecResult exec_stmt(Stmt* s); /* fwd */

static Value eval_bin_num(Value L, int op, Value R){
  if(op=='+'||op=='-'||op=='*'||op=='/'||op=='%'){
    if(!(is_numeric(L.type)&&is_numeric(R.type))) return V_void();
    if(op=='/'){
      float lv=(L.type==TY_FLOAT||L.type==TY_DOUBLE?L.u.fval:L.u.ival);
      float rv=(R.type==TY_FLOAT||R.type==TY_DOUBLE?R.u.fval:R.u.ival);
      return V_float(lv/rv);
    }
    if(op=='*'){
      if(L.type==TY_FLOAT||L.type==TY_DOUBLE||R.type==TY_FLOAT||R.type==TY_DOUBLE)
        return V_float((L.type==TY_FLOAT||L.type==TY_DOUBLE?L.u.fval:L.u.ival)*
                       (R.type==TY_FLOAT||R.type==TY_DOUBLE?R.u.fval:R.u.ival));
      else return V_int(L.u.ival*R.u.ival);
    }
    if(op=='+'){
      if(L.type==TY_FLOAT||L.type==TY_DOUBLE||R.type==TY_FLOAT||R.type==TY_DOUBLE)
        return V_float((L.type==TY_FLOAT||L.type==TY_DOUBLE?L.u.fval:L.u.ival)+
                       (R.type==TY_FLOAT||R.type==TY_DOUBLE?R.u.fval:R.u.ival));
      else return V_int(L.u.ival+R.u.ival);
    }
    if(op=='-'){
      if(L.type==TY_FLOAT||L.type==TY_DOUBLE||R.type==TY_FLOAT||R.type==TY_DOUBLE)
        return V_float((L.type==TY_FLOAT||L.type==TY_DOUBLE?L.u.fval:L.u.ival)-
                       (R.type==TY_FLOAT||R.type==TY_DOUBLE?R.u.fval:R.u.ival));
      else return V_int(L.u.ival-R.u.ival);
    }
    if(op=='%'){ return V_int(L.u.ival % R.u.ival); }
  }
  if(op=='<'||op=='>'||op==OP_LTE||op==OP_GTE){
    float lv=(L.type==TY_FLOAT||L.type==TY_DOUBLE?L.u.fval:L.u.ival);
    float rv=(R.type==TY_FLOAT||R.type==TY_DOUBLE?R.u.fval:R.u.ival);
    if(op=='<') return V_bool(lv<rv);
    if(op=='>') return V_bool(lv>rv);
    if(op==OP_LTE) return V_bool(lv<=rv);
    if(op==OP_GTE) return V_bool(lv>=rv);
  }
  return V_void();
}

static Value eval_expr(Expr* e){
  if(!e) return V_void();
  switch(e->kind){
    case EK_INT:    return V_int(e->u.ival);
    case EK_FLOAT:  return V_float(e->u.fval);
    case EK_CHAR:   return V_char(e->u.cval);
    case EK_STRING: return V_string(e->u.sval?e->u.sval:"");
    case EK_BOOL:   return V_bool(e->u.ival);
    case EK_NULL:   return V_null();
    case EK_ID: {
      Sym* s=st_find(e->u.id);
      if(!s) return V_void();
      if(s->val.type==TY_STRING) return V_string(s->val.u.sval);
      return s->val;
    }
    case EK_UNOP: {
      Value v = eval_expr(e->u.unop.e);
      if(e->u.unop.op=='-'){
        if(v.type==TY_INT) return V_int(-v.u.ival);
        if(v.type==TY_FLOAT || v.type==TY_DOUBLE) return V_float(-v.u.fval);
      } else if(e->u.unop.op==OP_NOT){
        return V_bool(! (v.type==TY_BOOL ? v.u.bval : 0));
      }
      return V_void();
    }
    case EK_BINOP: {
      int op=e->u.binop.op;
      if(op=='+' && (e->u.binop.l->type==TY_STRING || e->u.binop.r->type==TY_STRING)){
        char* a = value_to_cstr(eval_expr(e->u.binop.l));
        char* b = value_to_cstr(eval_expr(e->u.binop.r));
        char* c = str_cat(a,b); free(a); free(b);
        Value v=V_string(c); free(c); return v;
      }
      if(op==OP_AND||op==OP_OR){
        Value L=eval_expr(e->u.binop.l);
        if(op==OP_AND){ if(!(L.type==TY_BOOL && L.u.bval)) return V_bool(0); Value R=eval_expr(e->u.binop.r); return V_bool(R.type==TY_BOOL && R.u.bval); }
        else { if(L.type==TY_BOOL && L.u.bval) return V_bool(1); Value R=eval_expr(e->u.binop.r); return V_bool(R.type==TY_BOOL && R.u.bval); }
      }
      if(op==OP_EQ||op==OP_NEQ){
        Value L=eval_expr(e->u.binop.l), R=eval_expr(e->u.binop.r);
        int eq=0;
        if(L.type==TY_STRING && R.type==TY_STRING) eq = (strcmp(L.u.sval?L.u.sval:"", R.u.sval?R.u.sval:"")==0);
        else if(is_numeric(L.type)&&is_numeric(R.type)){
          float lv=(L.type==TY_FLOAT||L.type==TY_DOUBLE?L.u.fval:L.u.ival);
          float rv=(R.type==TY_FLOAT||R.type==TY_DOUBLE?R.u.fval:R.u.ival);
          eq = (lv==rv);
        } else if(L.type==TY_BOOL && R.type==TY_BOOL) eq = (L.u.bval==R.u.bval);
        else if(L.type==TY_CHAR && R.type==TY_CHAR) eq = (L.u.cval==R.u.cval);
        else if((L.type==TY_NULL && (R.type==TY_NULL||R.type==TY_STRING)) || (R.type==TY_NULL && (L.type==TY_NULL||L.type==TY_STRING))){
          eq = (L.type==TY_NULL && R.type==TY_NULL);
        }
        return V_bool(op==OP_EQ ? eq : !eq);
      }
      if(is_numeric(e->u.binop.l->type) && is_numeric(e->u.binop.r->type)){
        int bop = e->u.binop.op;
        return eval_bin_num(eval_expr(e->u.binop.l), bop, eval_expr(e->u.binop.r));
      }
      return V_void();
    }
    case EK_MCALL: {
      if (e->u.mcall.mname && strcmp(e->u.mcall.mname, "equals") == 0) {
        Value recv = eval_expr(e->u.mcall.recv);
        ArgList* a = e->u.mcall.args;
        Value arg0 = a ? eval_expr(a->e) : V_void();
        int ok = (recv.type==TY_STRING && arg0.type==TY_STRING);
        return V_bool(ok ? (strcmp(recv.u.sval?recv.u.sval:"", arg0.u.sval?arg0.u.sval:"")==0) : 0);
      }
      return V_void();
    }
    case EK_VALUEOF: {
      Value x = eval_expr(e->u.one.e);
      char* s = value_to_cstr(x); Value r = V_string(s); free(s); return r;
    }
    case EK_PARSEINT: {
      Value x = eval_expr(e->u.one.e);
      if(x.type==TY_STRING && is_int_string(x.u.sval)) return V_int(atoi(x.u.sval));
      return V_int(0);
    }
    case EK_PARSEFLOAT:
    case EK_PARSEDOB: {
      Value x = eval_expr(e->u.one.e);
      if(x.type==TY_STRING && is_float_string(x.u.sval)) return V_float((float)atof(x.u.sval));
      return V_float(0.0f);
    }
    case EK_JOIN: {
      Value d = eval_expr(e->u.join.delim);
      const char* delim = (d.type==TY_STRING && d.u.sval)? d.u.sval : "";
      char* acc = strdup("");
      ArgList* p = e->u.join.elems; int first=1;
      while(p){
        Value ve = eval_expr(p->e);
        char* piece = value_to_cstr(ve);
        char* old = acc;
        if(!first){ acc = str_cat(old, delim); free(old); old=acc; }
        acc = str_cat(old, piece);
        free(old); free(piece);
        first=0; p=p->next;
      }
      Value r = V_string(acc); free(acc); return r;
    }
    case EK_CALL: {
      Func* f = f_find(e->u.call.fname);
      if(!f){ return V_void(); }

      int n=0; for(ArgList* p=e->u.call.args;p;p=p->next) n++;
      Value* argv = (Value*)calloc(n>0?n:1, sizeof(Value));
      int i=0; for(ArgList* p=e->u.call.args;p;p=p->next){ argv[i++] = eval_expr(p->e); }

      st_push_scope_named(e->u.call.fname);
      for(i=0;i<f->arity && i<n;i++){
        Sym* s = st_insert_sym_pos_here(f->pnames[i], f->params[i], "Variable", 0, 0);
        Value v = argv[i];
        if(s){
          if((s->type==TY_FLOAT || s->type==TY_DOUBLE) && v.type==TY_INT) v = V_float((float)v.u.ival);
          value_free(&s->val);
          s->val = (v.type==TY_STRING? V_string(v.u.sval): v);
        }
      }

      ExecResult er = exec_stmt(f->body);
      st_pop_scope();
      free(argv);

      if(er.sig==ES_RETURN) return er.ret;
      return V_void();
    }
  }
  return V_void();
}

static ExecResult exec_stmt(Stmt* s){
  ExecResult R; R.sig=ES_NONE; R.ret=V_void();
  if(!s) return R;
  switch(s->kind){
    case SK_BLOCK: {
      st_push_scope();
      for(StmtList* p=s->u.block.list; p; p=p->next){
        ExecResult x = exec_stmt(p->s);
        if(x.sig!=ES_NONE){ st_pop_scope(); return x; }
      }
      st_pop_scope();
      return R;
    }
    case SK_SEQ: {
      for(StmtList* p=s->u.block.list; p; p=p->next){
        ExecResult x = exec_stmt(p->s);
        if(x.sig!=ES_NONE) return x;
      }
      return R;
    }
    case SK_DECL: {
      Sym* sym = st_insert_sym_pos_here(s->u.decl.id, s->u.decl.t, "Variable", 0, 0);
      if(s->u.decl.init){
        Value v = eval_expr(s->u.decl.init);
        if(sym){
          if((sym->type==TY_FLOAT || sym->type==TY_DOUBLE) && v.type==TY_INT){ v = V_float((float)v.u.ival); }
          value_free(&sym->val);
          sym->val = v.type==TY_STRING? V_string(v.u.sval): v;
        }
      }
      return R;
    }
    case SK_ASSIGN: {
      Sym* sym = st_find(s->u.assign.id);
      if(sym){
        Value v = eval_expr(s->u.assign.rhs);
        if((sym->type==TY_FLOAT || sym->type==TY_DOUBLE) && v.type==TY_INT) v = V_float((float)v.u.ival);
        value_free(&sym->val);
        sym->val = (v.type==TY_STRING? V_string(v.u.sval): v);
      }
      return R;
    }
    case SK_PRINT: {
      Value v = eval_expr(s->u.print.e);
      char* srepr = value_to_cstr(v);
      out_append(srepr);
      free(srepr);
      return R;
    }
    case SK_EXPRSTMT: { (void)eval_expr(s->u.exprstmt.e); return R; }
    case SK_IF: {
      Value c = eval_expr(s->u.sif.cond);
      int truth = (c.type==TY_BOOL? c.u.bval : 0);
      ExecResult x = exec_stmt(truth? s->u.sif.thenS : s->u.sif.elseS);
      if(x.sig!=ES_NONE) return x;
      return R;
    }
    case SK_WHILE: {
      while(1){
        Value c = eval_expr(s->u.swhile.cond);
        if(!(c.type==TY_BOOL && c.u.bval)) break;
        ExecResult x = exec_stmt(s->u.swhile.body);
        if(x.sig==ES_RETURN) return x;
        if(x.sig==ES_BREAK) break;
        if(x.sig==ES_CONTINUE) continue;
      }
      return R;
    }
    case SK_DOWHILE: {
      do {
        ExecResult x = exec_stmt(s->u.sdowhile.body);
        if(x.sig==ES_RETURN) return x;
        if(x.sig==ES_BREAK) break;
        if(x.sig==ES_CONTINUE){ }
        Value c = eval_expr(s->u.sdowhile.cond);
        if(!(c.type==TY_BOOL && c.u.bval)) break;
      } while(1);
      return R;
    }
    case SK_FOR: {
      (void)exec_stmt(s->u.sfor.init);
      while(1){
        Value c = eval_expr(s->u.sfor.cond);
        if(!(c.type==TY_BOOL && c.u.bval)) break;
        ExecResult x = exec_stmt(s->u.sfor.body);
        if(x.sig==ES_RETURN) return x;
        if(x.sig==ES_BREAK) break;
        if(x.sig==ES_CONTINUE){ (void)exec_stmt(s->u.sfor.post); continue; }
        (void)exec_stmt(s->u.sfor.post);
      }
      return R;
    }
    case SK_SWITCH: {
      Value sv = eval_expr(s->u.sswitch.sw);
      int matched = 0;
      CaseBlock* start = NULL;
      for(CaseBlock* c=s->u.sswitch.cases; c; c=c->next){
        if(!matched){
          Value lv = eval_expr(c->label);
          if(values_equal(sv, lv)){ matched=1; start=c; break; }
        }
      }
      if(matched){
        for(CaseBlock* c=start; c; c=c->next){
          for(StmtList* p=c->body; p; p=p->next){
            ExecResult x = exec_stmt(p->s);
            if(x.sig==ES_RETURN) return x;
            if(x.sig==ES_BREAK){ R.sig=ES_NONE; return R; }
            if(x.sig==ES_CONTINUE) return x;
          }
        }
        return R;
      } else {
        for(StmtList* p=s->u.sswitch.deflt; p; p=p->next){
          ExecResult x = exec_stmt(p->s);
          if(x.sig==ES_RETURN) return x;
          if(x.sig==ES_BREAK){ R.sig=ES_NONE; return R; }
          if(x.sig==ES_CONTINUE) return x;
        }
        return R;
      }
    }
    case SK_RETURN: {
      R.sig = ES_RETURN;
      R.ret = s->u.sret.e ? eval_expr(s->u.sret.e) : V_void();
      return R;
    }
    case SK_BREAK:    { R.sig=ES_BREAK; return R; }
    case SK_CONTINUE: { R.sig=ES_CONTINUE; return R; }
  }
  return R;
}

/* ============== Construcción de AST en las acciones ============== */
%}
%define parse.error verbose
%locations

%union {
  int    ival;
  float  fval;
  char   cval;
  char*  sval;
  int    typ;

  struct Expr* expr;
  struct Stmt* stmt;
  struct StmtList* slist;
  struct ArgList* alist;
  struct CaseBlock* clist;
}

/* tokens base */
%token T_PUBLIC T_STATIC T_VOID T_ID
%token T_INT T_FLOAT T_DOUBLE T_BOOLEAN T_CHAR T_STRING
%token T_TRUE T_FALSE T_NULL
%token T_INT_LIT T_FLOAT_LIT T_CHAR_LIT T_STRING_LIT
%token T_PRINTLN
%token T_IF T_ELSE
%token T_WHILE T_DO
%token T_FOR
%token T_SWITCH T_CASE T_DEFAULT T_BREAK T_CONTINUE
%token T_RETURN
%token T_INC T_DEC
%token T_PARSE_INT T_PARSE_FLOAT T_PARSE_DOUBLE
%token T_VALUEOF
%token T_JOIN

/* operadores compuestos y relacionales/lógicos */
%token T_EQ T_NEQ T_GTE T_LTE
%token T_AND T_OR T_NOT
%token T_ADDEQ T_SUBEQ T_MULEQ T_DIVEQ T_MODEQ
%token T_ANDEQ T_OREQ T_XOREQ T_SHREQ T_SHLEQ

/* Tipos de yylval por token / no-tokens */
%type <sval> T_ID T_STRING_LIT
%type <ival> T_INT_LIT
%type <fval> T_FLOAT_LIT
%type <cval> T_CHAR_LIT

/* === tipos semánticos === */
%type <typ>  tipo
%type <typ>  ret_type

/* AST: expresiones y sentencias */
%type <expr> expresion
%type <stmt> sentencia bloque if_stmt while_stmt dowhile_stmt for_stmt switch_stmt
%type <stmt> asignacion impresion return_stmt break_stmt continue_stmt
%type <stmt> for_simple_init for_simple_post
%type <slist> lista_sentencias
%type <slist> lista_declaradores
%type <stmt>  declaracion
%type <stmt>  declarador

/* ++/-- como sentencia */
%type <stmt>  incdec_stmt

/* switch helpers */
%type <clist> case_list case_list_opt
%type <slist> default_opt


/* listas para llamadas/join */
%type <alist> arglist_opt arglist join_elems

/* parámetros de funciones (conteo entero) */
%type <ival> params_opt
%type <ival> param_list

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

programa
  : lista_funciones
    {
      /* 1) Si NO hay errores, podemos ejecutar y sacar AST/TS */
      if(semantic_errors==0){
        Func* m = f_find("main");
        if(!m){
          log_reset();
          log_append("[Runtime] No se encontró función 'main'");
        } else {
          out_reset();
          ExecResult er = exec_stmt(m->body);
          (void)er;

          /* AST y Tabla de símbolos */
          const char* p  = parser_write_ast_txt("AST.txt");
          if(p){ LOGF("Reporte AST escrito en: %s", p); }

          const char* ps = parser_write_symbols_txt("TablaSimbolos.txt");
          if(ps){ LOGF("Tabla de símbolos escrita en: %s", ps); }

        }
        log_reset();
        log_append("OK: parse y ejecución completados");
      } else {
        /* 2) Si hay errores, NO ejecutamos, pero avisamos en el log */
        log_append("Se detectaron errores; no se ejecutó 'main'.");
      }

      /* 3) SIEMPRE escribir el reporte de errores, haya o no errores */
      {
        const char* pe = parser_write_errors_txt("Errores.txt");
        if(pe){ LOGF("Reporte de errores escrito en: %s", pe); }
      }
    }
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

/* --- funciones: predeclaración para recursividad --- */
params_opt
  : /* vacío */        { fn_arity = 0; $$ = 0; }
  | param_list         { $$ = $1; }
  ;

param_list
  : tipo T_ID
      { if(is_reserved($2)) semf("[Semántico] Identificador inválido como parámetro: '%s'", $2);
        else { 
          st_insert_sym_pos_here($2,(Type)$1,"Variable", @2.first_line, @2.first_column);
          if(fn_arity<64){ fn_params[fn_arity] = (Type)$1; fn_param_names[fn_arity] = strdup($2); fn_arity++; }
        }
        $$ = 1;
      }
  | param_list ',' tipo T_ID
      { if(is_reserved($4)) semf("[Semántico] Identificador inválido como parámetro: '%s'", $4);
        else { 
          st_insert_sym_pos_here($4,(Type)$3,"Variable", @4.first_line, @4.first_column);
          if(fn_arity<64){ fn_params[fn_arity] = (Type)$3; fn_param_names[fn_arity] = strdup($4); fn_arity++; }
        }
        $$ = $1 + 1;
      }
  ;

func_decl
  : opt_pubstat ret_type T_ID '('
      {
        current_func_ret = (Type)$2; func_depth++; fn_arity = 0;
        (void)f_insert_placeholder($3, (Type)$2);

        /* registrar la función en TS con ámbito Global */
        Scope* save = current_scope;
        while(current_scope && current_scope->parent) current_scope = current_scope->parent; /* ir a Global */
        st_insert_sym_pos_here($3,(Type)$2,"Función", @3.first_line, @3.first_column);
        current_scope = save;

        /* ámbito interno con nombre de la función */
        st_push_scope_named($3);
      }
      params_opt ')'
      { loop_depth=0; switch_depth=0; }
      '{' lista_sentencias '}'
      {
        Func* f = f_find($3);
        if(f){
          f->ret = (Type)$2;
          f->arity = fn_arity;
          if(fn_arity>0){
            f->params = (Type*)malloc(sizeof(Type)*fn_arity);
            memcpy(f->params, fn_params, sizeof(Type)*fn_arity);
            f->pnames = (char**)calloc(fn_arity, sizeof(char*));
            for(int i=0;i<fn_arity;i++) f->pnames[i] = strdup(fn_param_names[i]);
          }
          f->body = S_new(SK_BLOCK);
          f->body->u.block.list = $10;
        }
        st_pop_scope(); func_depth--; current_func_ret = TY_UNDEF;
      }
  ;

/* ===== Sentencias ===== */
lista_sentencias
  : /* vacío */                 { $$ = NULL; }
  | lista_sentencias sentencia  { $$ = SL_push($1, $2); }
  ;

sentencia
  : declaracion
  | asignacion ';'
  | incdec_stmt ';'
  | impresion ';'
  | bloque
  | if_stmt
  | while_stmt
  | dowhile_stmt
  | for_stmt
  | switch_stmt
  | return_stmt
  | break_stmt
  | continue_stmt
  | expresion ';'     { Stmt* s=S_new(SK_EXPRSTMT); s->u.exprstmt.e=$1; $$=s; }
  ;

bloque
  : '{' { st_push_scope(); } lista_sentencias '}' { st_pop_scope(); Stmt* s=S_new(SK_BLOCK); s->u.block.list=$3; $$=s; }
  ;

/* ===== If / Else ===== */
if_stmt
  : T_IF '(' expresion ')' sentencia %prec LOWER_THAN_ELSE
      { Stmt* s=S_new(SK_IF); s->u.sif.cond=$3; s->u.sif.thenS=$5; s->u.sif.elseS=NULL;
        if($3->type!=TY_BOOL) semf("[Semántico] La condición del if debe ser boolean (recibido %s)", tname($3->type));
        $$=s; }
  | T_IF '(' expresion ')' sentencia T_ELSE sentencia
      { Stmt* s=S_new(SK_IF); s->u.sif.cond=$3; s->u.sif.thenS=$5; s->u.sif.elseS=$7;
        if($3->type!=TY_BOOL) semf("[Semántico] La condición del if debe ser boolean (recibido %s)", tname($3->type));
        $$=s; }
  ;

/* ===== While ===== */
enter_loop : /* vacío */ { loop_depth++; } ;

while_stmt
  : T_WHILE '(' expresion ')' enter_loop sentencia
      {
        if ($3->type != TY_BOOL)
          semf("[Semántico] La condición del while debe ser boolean (recibido %s)", tname($3->type));
        Stmt* s = S_new(SK_WHILE);
        s->u.swhile.cond = $3;
        s->u.swhile.body = $6;
        $$ = s;
        loop_depth--; if (loop_depth < 0) loop_depth = 0;
      }
  ;

/* ===== Do-While ===== */
dowhile_stmt
  : T_DO enter_loop sentencia T_WHILE '(' expresion ')' ';'
      {
        if ($6->type != TY_BOOL)
          semf("[Semántico] La condición del do-while debe ser boolean (recibido %s)", tname($6->type));
        Stmt* s = S_new(SK_DOWHILE);
        s->u.sdowhile.cond = $6;
        s->u.sdowhile.body = $3;
        $$ = s;
        loop_depth--; if (loop_depth < 0) loop_depth = 0;
      }
  ;

/* ===== For clásico ===== */
for_stmt
  : T_FOR '(' for_simple_init ';' expresion ';' for_simple_post ')' enter_loop sentencia
      {
        if ($5->type != TY_BOOL)
          semf("[Semántico] La condición del for debe ser boolean (recibido %s)", tname($5->type));
        Stmt* s = S_new(SK_FOR);
        s->u.sfor.init = $3;
        s->u.sfor.cond = $5;
        s->u.sfor.post = $7;
        s->u.sfor.body = $10;
        $$ = s;
        loop_depth--; if (loop_depth < 0) loop_depth = 0;
      }
  ;

for_simple_init
  : tipo { current_decl_type = (Type)$1; } lista_declaradores
      { Stmt* seq = S_new(SK_SEQ); seq->u.block.list = $3; $$ = seq; }
  | asignacion              { $$ = $1; }
  | /* vacío */             { $$ = NULL; }
  ;

for_simple_post
  : asignacion              { $$ = $1; }
  | incdec_stmt             { $$ = $1; }
  | /* vacío */             { $$ = NULL; }
  ;

/* ===== Switch / Case ===== */
switch_stmt
  : T_SWITCH '(' expresion ')' { switch_depth++; } '{' case_list_opt default_opt '}' { switch_depth--; 
      Stmt* s=S_new(SK_SWITCH); s->u.sswitch.sw=$3; s->u.sswitch.cases=$7; s->u.sswitch.deflt=$8;
      if(!is_numeric($3->type) && $3->type!=TY_CHAR && $3->type!=TY_STRING)
        semf("[Semántico] switch no soporta tipo %s", tname($3->type));
      $$=s; }
  ;

case_list_opt
  : /* vacío */ { $$=NULL; }
  | case_list   { $$=$1; }
  ;

case_list
  : case_list T_CASE expresion ':' lista_sentencias
      { $$ = CL_push($1, $3, $5); }
  | T_CASE expresion ':' lista_sentencias
      { $$ = CL_push(NULL, $2, $4); }
  ;

default_opt
  : /* vacío */ { $$=NULL; }
  | T_DEFAULT ':' lista_sentencias { $$=$3; }
  ;

/* ===== Asignaciones ===== */
asignacion
  : T_ID '=' expresion
      {
        Sym* s=st_find($1);
        if(!s) semf("[Semántico] Uso de variable no declarada: %s", $1);
        else if(!compatible_assign(s->type,$3->type))
          semf("[Semántico] No compatible: %s = %s", tname(s->type), tname($3->type));
        Stmt* st=S_new(SK_ASSIGN);
        st->u.assign.id=$1; st->u.assign.op='=';
        st->u.assign.rhs=$3;
        $$=st;
      }
  | T_ID T_ADDEQ expresion
      {
        Sym* s=st_find($1);
        if(!s) semf("[Semántico] Uso de variable no declarada: %s", $1);
        if(!(is_numeric(s?s->type:TY_ERROR) && is_numeric($3->type)))
          semf("[Semántico] '+= requiere numéricos");
        Expr* left = E_new(EK_ID); left->u.id=$1; left->type=s?s->type:TY_ERROR;
        Expr* rhs  = E_new(EK_BINOP);
        rhs->u.binop.op='+'; rhs->u.binop.l=left; rhs->u.binop.r=$3;
        rhs->type = promote_num(left->type,$3->type);
        Stmt* st=S_new(SK_ASSIGN);
        st->u.assign.id=$1; st->u.assign.op='=';
        st->u.assign.rhs=rhs;
        $$=st;
      }
  | T_ID T_SUBEQ expresion
      {
        Sym* s=st_find($1);
        if(!s) semf("[Semántico] Uso de variable no declarada: %s", $1);
        if(!(is_numeric(s?s->type:TY_ERROR) && is_numeric($3->type)))
          semf("[Semántico] '-= requiere numéricos");
        Expr* left = E_new(EK_ID); left->u.id=$1; left->type=s?s->type:TY_ERROR;
        Expr* rhs  = E_new(EK_BINOP);
        rhs->u.binop.op='-'; rhs->u.binop.l=left; rhs->u.binop.r=$3;
        rhs->type = promote_num(left->type,$3->type);
        Stmt* st=S_new(SK_ASSIGN);
        st->u.assign.id=$1; st->u.assign.op='=';
        st->u.assign.rhs=rhs;
        $$=st;
      }
  | T_ID T_MULEQ expresion
      {
        Sym* s=st_find($1);
        if(!s) semf("[Semántico] Uso de variable no declarada: %s", $1);
        if(!(is_numeric(s?s->type:TY_ERROR) && is_numeric($3->type)))
          semf("[Semántico] '*=' requiere numéricos");
        Expr* left = E_new(EK_ID); left->u.id=$1; left->type=s?s->type:TY_ERROR;
        Expr* rhs  = E_new(EK_BINOP);
        rhs->u.binop.op='*'; rhs->u.binop.l=left; rhs->u.binop.r=$3;
        rhs->type = promote_num(left->type,$3->type);
        Stmt* st=S_new(SK_ASSIGN);
        st->u.assign.id=$1; st->u.assign.op='=';
        st->u.assign.rhs=rhs;
        $$=st;
      }
  | T_ID T_DIVEQ expresion
      {
        Sym* s=st_find($1);
        if(!s) semf("[Semántico] Uso de variable no declarada: %s", $1);
        if(!(is_numeric(s?s->type:TY_ERROR) && is_numeric($3->type)))
          semf("[Semántico] '/=' requiere numéricos");
        Expr* left = E_new(EK_ID); left->u.id=$1; left->type=s?s->type:TY_ERROR;
        Expr* rhs  = E_new(EK_BINOP);
        rhs->u.binop.op='/'; rhs->u.binop.l=left; rhs->u.binop.r=$3;
        rhs->type = TY_FLOAT;
        Stmt* st=S_new(SK_ASSIGN);
        st->u.assign.id=$1; st->u.assign.op='=';
        st->u.assign.rhs=rhs;
        $$=st;
      }
  | T_ID T_MODEQ expresion
      {
        Sym* s=st_find($1);
        if(!s) semf("[Semántico] Uso de variable no declarada: %s", $1);
        if(!((s?s->type:TY_ERROR)==TY_INT && $3->type==TY_INT))
          semf("[Semántico] '%=' requiere int %% int");
        Expr* left = E_new(EK_ID); left->u.id=$1; left->type=s?s->type:TY_ERROR;
        Expr* rhs  = E_new(EK_BINOP);
        rhs->u.binop.op='%'; rhs->u.binop.l=left; rhs->u.binop.r=$3;
        rhs->type = TY_INT;
        Stmt* st=S_new(SK_ASSIGN);
        st->u.assign.id=$1; st->u.assign.op='=';
        st->u.assign.rhs=rhs;
        $$=st;
      }
  ;


/* ===== ++ / -- como sentencia ===== */
incdec_stmt
  : T_ID T_INC
      {
        Sym* s=st_find($1);
        if(!s) semf("[Semántico] Uso de variable no declarada: %s", $1);
        if(!(s && is_numeric(s->type))) semf("[Semántico] '++' requiere variable numérica");
        Expr* left = E_new(EK_ID); left->u.id=$1; left->type=s? s->type:TY_ERROR;
        Expr* one  = E_new(EK_INT); one->type=TY_INT; one->u.ival=1;
        Expr* rhs  = E_new(EK_BINOP); rhs->u.binop.op='+'; rhs->u.binop.l=left; rhs->u.binop.r=one; rhs->type=promote_num(left->type, TY_INT);
        Stmt* st = S_new(SK_ASSIGN); st->u.assign.id=$1; st->u.assign.op='='; st->u.assign.rhs=rhs; $$=st;
      }
  | T_ID T_DEC
      {
        Sym* s=st_find($1);
        if(!s) semf("[Semántico] Uso de variable no declarada: %s", $1);
        if(!(s && is_numeric(s->type))) semf("[Semántico] '--' requiere variable numérica");
        Expr* left = E_new(EK_ID); left->u.id=$1; left->type=s? s->type:TY_ERROR;
        Expr* one  = E_new(EK_INT); one->type=TY_INT; one->u.ival=1;
        Expr* rhs  = E_new(EK_BINOP); rhs->u.binop.op='-'; rhs->u.binop.l=left; rhs->u.binop.r=one; rhs->type=promote_num(left->type, TY_INT);
        Stmt* st = S_new(SK_ASSIGN); st->u.assign.id=$1; st->u.assign.op='='; st->u.assign.rhs=rhs; $$=st;
      }
  ;

/* ===== Declaraciones ===== */
declaracion
  : tipo { current_decl_type = (Type)$1; } lista_declaradores ';'
      { Stmt* seq=S_new(SK_SEQ); seq->u.block.list = $3; $$ = seq; }
  ;

lista_declaradores
  : declarador                         { $$ = SL_push(NULL, $1); }
  | lista_declaradores ',' declarador  { $$ = SL_push($1, $3); }
  ;

declarador
  : T_ID
      {
        if(is_reserved($1)) semf("[Semántico] Identificador inválido: '%s' es palabra reservada", $1);
        st_insert_sym_pos_here($1, current_decl_type, "Variable", @1.first_line, @1.first_column);
        Stmt* s=S_new(SK_DECL); s->u.decl.id=$1; s->u.decl.t=current_decl_type; s->u.decl.init=NULL; $$=s; }
  | T_ID '=' expresion
      {
        if(is_reserved($1)) semf("[Semántico] Identificador inválido: '%s' es palabra reservada", $1);
        else if(!compatible_assign(current_decl_type,$3->type))
          semf("[Semántico] No compatible: %s = %s", tname(current_decl_type), tname($3->type));
        st_insert_sym_pos_here($1, current_decl_type, "Variable", @1.first_line, @1.first_column);
        Stmt* s=S_new(SK_DECL); s->u.decl.id=$1; s->u.decl.t=current_decl_type; s->u.decl.init=$3; $$=s; }
  ;

tipo
  : T_INT      { $$ = TY_INT; }
  | T_FLOAT    { $$ = TY_FLOAT; }
  | T_DOUBLE   { $$ = TY_DOUBLE; }
  | T_BOOLEAN  { $$ = TY_BOOL; }
  | T_CHAR     { $$ = TY_CHAR; }
  | T_STRING   { $$ = TY_STRING; }
  ;

/* ===== return/break/continue ===== */
return_stmt
  : T_RETURN ';'
      { if(func_depth<=0) semf("[Semántico] 'return' fuera de función");
        else if(current_func_ret != TY_VOID) semf("[Semántico] 'return;' en función no-void (%s)", tname(current_func_ret));
        Stmt* s=S_new(SK_RETURN); s->u.sret.e=NULL; $$=s; }
  | T_RETURN expresion ';'
      { if(func_depth<=0) semf("[Semántico] 'return' fuera de función");
        else if(current_func_ret == TY_VOID) semf("[Semántico] 'return expr;' en función void");
        else if(!compatible_assign(current_func_ret,$2->type))
          semf("[Semántico] return incompatible: se esperaba %s y se obtuvo %s", tname(current_func_ret), tname($2->type));
        Stmt* s=S_new(SK_RETURN); s->u.sret.e=$2; $$=s; }
  ;

break_stmt
  : T_BREAK ';'
      { if(loop_depth<=0 && switch_depth<=0) semf("[Semántico] 'break' fuera de bucle/switch");
        Stmt* s=S_new(SK_BREAK); $$=s; }
  ;

continue_stmt
  : T_CONTINUE ';'
      { if(loop_depth<=0) semf("[Semántico] 'continue' fuera de bucle");
        Stmt* s=S_new(SK_CONTINUE); $$=s; }
  ;

/* ===== Print ===== */
impresion
  : T_PRINTLN '(' expresion ')'
      { if ($3->type == TY_ERROR) semf("[Semántico] println recibe expresión inválida");
        Stmt* s=S_new(SK_PRINT); s->u.print.e=$3; $$=s; }
  ;

/* ===== Llamadas a función dentro de expresiones ===== */
arglist_opt : /* vacío */ { $$ = NULL; } | arglist { $$ = $1; } ;
arglist
  : expresion                    { $$ = AL_push(NULL, $1); }
  | arglist ',' expresion        { $$ = AL_push($1, $3); }
  ;

/* ===== Expresiones ===== */
join_elems
  : expresion                    { $$ = AL_push(NULL, $1); }
  | join_elems ',' expresion     { $$ = AL_push($1, $3); }
  ;

expresion
  : T_INT_LIT        { Expr* e=E_new(EK_INT); e->type=TY_INT; e->u.ival=$1; $$=e; }
  | T_FLOAT_LIT      { Expr* e=E_new(EK_FLOAT); e->type=TY_FLOAT; e->u.fval=$1; $$=e; }
  | T_CHAR_LIT       { Expr* e=E_new(EK_CHAR); e->type=TY_CHAR; e->u.cval=$1; $$=e; }
  | T_STRING_LIT     { Expr* e=E_new(EK_STRING); e->type=TY_STRING; e->u.sval=unquote($1); free($1); $$=e; }
  | T_TRUE           { Expr* e=E_new(EK_BOOL); e->type=TY_BOOL; e->u.ival=1; $$=e; }
  | T_FALSE          { Expr* e=E_new(EK_BOOL); e->type=TY_BOOL; e->u.ival=0; $$=e; }
  | T_NULL           { Expr* e=E_new(EK_NULL); e->type=TY_NULL; $$=e; }
  | T_ID             { Sym* s=st_find($1); if(!s){ semf("[Semántico] Uso de variable no declarada: %s", $1); }
                       Expr* e=E_new(EK_ID); e->type = s? s->type: TY_ERROR; e->u.id=$1; $$=e; }
  | '(' expresion ')' { $$ = $2; }
  | '-' expresion %prec UMINUS { if(!is_numeric($2->type)) semf("'-' requiere numérico"); Expr* e=E_new(EK_UNOP); e->u.unop.op='-'; e->u.unop.e=$2; e->type=$2->type; $$=e; }
  | T_NOT expresion            { if($2->type!=TY_BOOL) semf("'!' requiere boolean"); Expr* e=E_new(EK_UNOP); e->u.unop.op=OP_NOT; e->u.unop.e=$2; e->type=TY_BOOL; $$=e; }
  | expresion '*' expresion    { if(!(is_numeric($1->type)&&is_numeric($3->type))) semf("'*' requiere numéricos"); Expr* e=E_new(EK_BINOP); e->u.binop.op='*'; e->u.binop.l=$1; e->u.binop.r=$3; e->type=promote_num($1->type,$3->type); $$=e; }
  | expresion '/' expresion    { if(!(is_numeric($1->type)&&is_numeric($3->type))) semf("'/' requiere numéricos"); Expr* e=E_new(EK_BINOP); e->u.binop.op='/'; e->u.binop.l=$1; e->u.binop.r=$3; e->type=TY_FLOAT; $$=e; }
  | expresion '%' expresion    { if(!($1->type==TY_INT && $3->type==TY_INT)) semf("'%%' requiere int %% int"); Expr* e=E_new(EK_BINOP); e->u.binop.op='%'; e->u.binop.l=$1; e->u.binop.r=$3; e->type=TY_INT; $$=e; }
  | expresion '+' expresion
      { Type t = (is_numeric($1->type)&&is_numeric($3->type))? promote_num($1->type,$3->type) : ($1->type==TY_STRING||$3->type==TY_STRING? TY_STRING: TY_ERROR);
        if(t==TY_ERROR) semf("'+' inválido entre %s y %s", tname($1->type), tname($3->type));
        Expr* e=E_new(EK_BINOP); e->u.binop.op='+'; e->u.binop.l=$1; e->u.binop.r=$3; e->type=t; $$=e; }
  | expresion '-' expresion    { if(!(is_numeric($1->type)&&is_numeric($3->type))) semf("'-' requiere numéricos"); Expr* e=E_new(EK_BINOP); e->u.binop.op='-'; e->u.binop.l=$1; e->u.binop.r=$3; e->type=promote_num($1->type,$3->type); $$=e; }
  | expresion '<' expresion    { if(!(is_numeric($1->type)&&is_numeric($3->type))) semf("'<' requiere numéricos"); Expr* e=E_new(EK_BINOP); e->u.binop.op='<'; e->u.binop.l=$1; e->u.binop.r=$3; e->type=TY_BOOL; $$=e; }
  | expresion '>' expresion    { if(!(is_numeric($1->type)&&is_numeric($3->type))) semf("'>' requiere numéricos"); Expr* e=E_new(EK_BINOP); e->u.binop.op='>'; e->u.binop.l=$1; e->u.binop.r=$3; e->type=TY_BOOL; $$=e; }
  | expresion T_LTE expresion  { if(!(is_numeric($1->type)&&is_numeric($3->type))) semf("'<=' requiere numéricos"); Expr* e=E_new(EK_BINOP); e->u.binop.op=OP_LTE; e->u.binop.l=$1; e->u.binop.r=$3; e->type=TY_BOOL; $$=e; }
  | expresion T_GTE expresion  { if(!(is_numeric($1->type)&&is_numeric($3->type))) semf("'>=' requiere numéricos"); Expr* e=E_new(EK_BINOP); e->u.binop.op=OP_GTE; e->u.binop.l=$1; e->u.binop.r=$3; e->type=TY_BOOL; $$=e; }
  | expresion T_EQ expresion   { Expr* e=E_new(EK_BINOP); e->u.binop.op=OP_EQ; e->u.binop.l=$1; e->u.binop.r=$3; e->type=TY_BOOL; $$=e; }
  | expresion T_NEQ expresion  { Expr* e=E_new(EK_BINOP); e->u.binop.op=OP_NEQ; e->u.binop.l=$1; e->u.binop.r=$3; e->type=TY_BOOL; $$=e; }
  | expresion T_AND expresion  { if(!($1->type==TY_BOOL&&$3->type==TY_BOOL)) semf("'&&' requiere boolean"); Expr* e=E_new(EK_BINOP); e->u.binop.op=OP_AND; e->u.binop.l=$1; e->u.binop.r=$3; e->type=TY_BOOL; $$=e; }
  | expresion T_OR  expresion  { if(!($1->type==TY_BOOL&&$3->type==TY_BOOL)) semf("'||' requiere boolean"); Expr* e=E_new(EK_BINOP); e->u.binop.op=OP_OR;  e->u.binop.l=$1; e->u.binop.r=$3; e->type=TY_BOOL; $$=e; }

  | T_VALUEOF '(' expresion ')' { Expr* e=E_new(EK_VALUEOF); e->u.one.e=$3; e->type=TY_STRING; $$=e; }

  | T_PARSE_INT '(' T_STRING_LIT ')'
      { char* s=unquote($3);
        if(!is_int_string(s)) semf("[Semántico] Integer.parseInt: cadena no numérica");
        free(s);
        Expr* e=E_new(EK_PARSEINT); Expr* lit=E_new(EK_STRING); lit->type=TY_STRING; lit->u.sval=unquote($3); free($3);
        e->u.one.e=lit; e->type=TY_INT; $$=e; }
  | T_PARSE_INT '(' expresion ')'
      { if($3->type!=TY_STRING) semf("[Semántico] Integer.parseInt requiere String");
        Expr* e=E_new(EK_PARSEINT); e->u.one.e=$3; e->type=TY_INT; $$=e; }

  | T_PARSE_FLOAT '(' T_STRING_LIT ')'
      { char* s=unquote($3); if(!is_float_string(s)) semf("[Semántico] Float.parseFloat: formato no válido"); free(s);
        Expr* e=E_new(EK_PARSEFLOAT); Expr* lit=E_new(EK_STRING); lit->type=TY_STRING; lit->u.sval=unquote($3); free($3);
        e->u.one.e=lit; e->type=TY_FLOAT; $$=e; }
  | T_PARSE_FLOAT '(' expresion ')'
      { if($3->type!=TY_STRING) semf("[Semántico] Float.parseFloat requiere String");
        Expr* e=E_new(EK_PARSEFLOAT); e->u.one.e=$3; e->type=TY_FLOAT; $$=e; }

  | T_PARSE_DOUBLE '(' T_STRING_LIT ')'
      { char* s=unquote($3); if(!is_float_string(s)) semf("[Semántico] Double.parseDouble: formato no válido"); free(s);
        Expr* e=E_new(EK_PARSEDOB); Expr* lit=E_new(EK_STRING); lit->type=TY_STRING; lit->u.sval=unquote($3); free($3);
        e->u.one.e=lit; e->type=TY_FLOAT; $$=e; }
  | T_PARSE_DOUBLE '(' expresion ')'
      { if($3->type!=TY_STRING) semf("[Semántico] Double.parseDouble requiere String");
        Expr* e=E_new(EK_PARSEDOB); e->u.one.e=$3; e->type=TY_FLOAT; $$=e; }

  | T_JOIN '(' expresion ',' join_elems ')'
      { if($3->type!=TY_STRING) semf("[Semántico] String.join: el delimitador debe ser String");
        Expr* e=E_new(EK_JOIN); e->u.join.delim=$3; e->u.join.elems=$5; e->type=TY_STRING; $$=e; }

  | expresion '.' T_ID '(' arglist_opt ')'
      {
        Expr* e=E_new(EK_MCALL);
        e->u.mcall.recv = $1;
        e->u.mcall.mname = $3;
        e->u.mcall.args = $5;

        int nargs=0; ArgList* p=$5; while(p){ nargs++; p=p->next; }

        if(strcmp($3,"equals")==0){
          if($1->type!=TY_STRING) semf("[Semántico] String.equals: el receptor debe ser String (recibido %s)", tname($1->type));
          if(nargs!=1) semf("[Semántico] String.equals espera 1 argumento");
          else {
            ArgList* a=$5;
            if(a && a->e->type!=TY_STRING) semf("[Semántico] String.equals: el argumento debe ser String (recibido %s)", tname(a->e->type));
          }
          e->type = TY_BOOL;
        } else {
          semf("[Semántico] Método no soportado: %s", $3);
          e->type = TY_ERROR;
        }
        $$=e;
      }

  | T_ID '(' arglist_opt ')'
      {
        Func* f = f_find($1);
        int n=0; for(ArgList* p=$3; p; p=p->next) n++;
        if (!f) {
          semf("[Semántico] Llamada a función no declarada: %s", $1);
        } else if (f->body != NULL && n != f->arity) {
          semf("[Semántico] Número de argumentos a %s: se esperaban %d y se pasaron %d",
               f->name, f->arity, n);
        }
        Expr* e = E_new(EK_CALL);
        e->u.call.fname = $1;
        e->u.call.args  = $3;
        e->type = f ? f->ret : TY_ERROR;
        $$ = e;
      }
      

%%

void yyerror(const char* s){
  /* Mantén el prefijo para distinguir errores de sintaxis en el log */
  semf("[Syntax] %s", s);
}

