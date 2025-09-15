
%option noinput nounput yylineno
%{
#include "parser.tab.h"
#include <string.h>
#include <stdlib.h>

/* --- tracking de columnas para %locations --- */
static int curr_col = 1;
/* Para cada token consumido, actualiza yylloc y la columna actual */
#define YY_USER_ACTION \
  do { \
    yylloc.first_line   = yylineno; \
    yylloc.last_line    = yylineno; \
    yylloc.first_column = curr_col; \
    yylloc.last_column  = curr_col + (int)yyleng - 1; \
    curr_col += (int)yyleng; \
  } while(0);
%}






/* --- reglas auxiliares --- */
ws      [ \t\r]+
nl      \n
idstart [A-Za-z_]
idrest  [A-Za-z0-9_]
intlit  [0-9]+
float1  ([0-9]+\.[0-9]*([eE][+-]?[0-9]+)?)
float2  (\.[0-9]+([eE][+-]?[0-9]+)?)
float3  ([0-9]+[eE][+-]?[0-9]+)
chresc  (\\[nrt'\"\\])
charlit \'([^\\\n']|{chresc})\'
strlit  \"([^\\\n\"]|\\[nrt\"\\])*\"

%%

{ws}                 ;
{nl}    { curr_col = 1; /* yylineno ya lo aumenta flex */ }
"//".*               ;
"/\\*"(\\*+[^*/]|[^*])*"\\*/" ;

"int"        { return T_INT; }
"float"      { return T_FLOAT; }
"double"     { return T_DOUBLE; }   /* <-- agregado */
"boolean"    { return T_BOOLEAN; }
"char"       { return T_CHAR; }
"String.valueOf"     { return T_VALUEOF; }
"valueOf"            { return T_VALUEOF; }

"String.join"        { return T_JOIN; }
"join"               { return T_JOIN; }

"String"     { return T_STRING; }

"true"       { return T_TRUE; }
"false"      { return T_FALSE; }
"null"       { return T_NULL; }

"public"     { return T_PUBLIC; }
"static"     { return T_STATIC; }
"void"       { return T_VOID; }

"if"         { return T_IF; }
"else"       { return T_ELSE; }
"while"      { return T_WHILE; }
"do"        { return T_DO; }
"for"        { return T_FOR; }

"switch"     { return T_SWITCH; }
"case"       { return T_CASE; }
"default"    { return T_DEFAULT; }
"break"      { return T_BREAK; }
"continue"   { return T_CONTINUE; }
"return"     { return T_RETURN; }

"System.out.println" { return T_PRINTLN; }

"Integer.parseInt"   { return T_PARSE_INT; }
"parseInt"           { return T_PARSE_INT; }

"Float.parseFloat"   { return T_PARSE_FLOAT; }
"parseFloat"         { return T_PARSE_FLOAT; }

"Double.parseDouble" { return T_PARSE_DOUBLE; }
"parseDouble"        { return T_PARSE_DOUBLE; }

"++"  { return T_INC; }
"--"  { return T_DEC; }

"+="  { return T_ADDEQ; }
"-="  { return T_SUBEQ; }
"*="  { return T_MULEQ; }
"/="  { return T_DIVEQ; }
"%="  { return T_MODEQ; }
"&="  { return T_ANDEQ; }
"|="  { return T_OREQ; }
"^="  { return T_XOREQ; }
">>=" { return T_SHREQ; }
"<<=" { return T_SHLEQ; }

"==" { return T_EQ; }
"!=" { return T_NEQ; }
">=" { return T_GTE; }
"<=" { return T_LTE; }
"&&" { return T_AND; }
"||" { return T_OR; }
"!"  { return T_NOT; }

"="  { return '='; }
";"  { return ';'; }
","  { return ','; }
":"  { return ':'; }
"("  { return '('; }
")"  { return ')'; }
"{"  { return '{'; }
"}"  { return '}'; }
"+"  { return '+'; }
"-"  { return '-'; }
"*"  { return '*'; }
"/"  { return '/'; }
"%"  { return '%'; }
"<"  { return '<'; }
">"  { return '>'; }

{float1}|{float2}|{float3}   { yylval.fval = strtof(yytext, NULL); return T_FLOAT_LIT; }
{intlit}                      { yylval.ival = (int)strtol(yytext, NULL, 10); return T_INT_LIT; }

{charlit} {
  char c;
  if (yytext[1]=='\\') {
    switch(yytext[2]){
      case 'n': c='\n'; break;
      case 'r': c='\r'; break;
      case 't': c='\t'; break;
      case '\\': c='\\'; break;
      case '\"': c='\"'; break;
      case '\'': c='\''; break;
      default: c=yytext[2];
    }
  } else c = yytext[1];
  yylval.cval = c; return T_CHAR_LIT;
}

{strlit}                      { yylval.sval = strdup(yytext); return T_STRING_LIT; }

{idstart}{idrest}*            { yylval.sval = strdup(yytext); return T_ID; }
.                             { return yytext[0]; }

%%
int yywrap(void){ return 1; }