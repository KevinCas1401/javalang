#include <stdio.h>
#include <ctype.h>
#include "parser.tab.h"

/* provistos por flex */
int yylex(void);
extern char *yytext;

static const char* tname(int t){
  switch(t){
    case 0: return "EOF";
    case T_PUBLIC: return "T_PUBLIC";
    case T_STATIC: return "T_STATIC";
    case T_VOID: return "T_VOID";
    case T_ID: return "T_ID";
    case T_INT: return "T_INT";
    case T_FLOAT: return "T_FLOAT";
    case T_BOOLEAN: return "T_BOOLEAN";
    case T_CHAR: return "T_CHAR";
    case T_STRING: return "T_STRING";
    case T_TRUE: return "T_TRUE";
    case T_FALSE: return "T_FALSE";
    case T_INT_LIT: return "T_INT_LIT";
    case T_FLOAT_LIT: return "T_FLOAT_LIT";
    case T_CHAR_LIT: return "T_CHAR_LIT";
    case T_STRING_LIT: return "T_STRING_LIT";
    case T_PRINTLN: return "T_PRINTLN";
    case T_IF: return "T_IF";
    case T_ELSE: return "T_ELSE";
    case T_WHILE: return "T_WHILE";
    case T_FOR: return "T_FOR";
    case T_SWITCH: return "T_SWITCH";
    case T_CASE: return "T_CASE";
    case T_DEFAULT: return "T_DEFAULT";
    case T_BREAK: return "T_BREAK";
    case T_EQ: return "T_EQ";
    case T_NEQ:return "T_NEQ";
    case T_GTE:return "T_GTE";
    case T_LTE:return "T_LTE";
    case T_AND:return "T_AND";
    case T_OR: return "T_OR";
    case T_NOT:return "T_NOT";
    case T_ADDEQ:return "T_ADDEQ";
    case T_SUBEQ:return "T_SUBEQ";
    case T_MULEQ:return "T_MULEQ";
    case T_DIVEQ:return "T_DIVEQ";
    case T_MODEQ:return "T_MODEQ";
    case T_ANDEQ:return "T_ANDEQ";
    case T_OREQ:return "T_OREQ";
    case T_XOREQ:return "T_XOREQ";
    case T_SHREQ:return "T_SHREQ";
    case T_SHLEQ:return "T_SHLEQ";
    default: {
      static char buf[32];
      if (t > 0 && t < 128 && isprint(t)) snprintf(buf, sizeof(buf), "'%c'", t);
      else snprintf(buf, sizeof(buf), "tok(%d)", t);
      return buf;
    }
  }
}

int main(void){
  int t;
  while ((t = yylex()) != 0) {
    printf("%-14s  [%s]\n", tname(t), yytext);
  }
  puts("EOF");
  return 0;
}