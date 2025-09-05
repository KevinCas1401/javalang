#include <stdio.h>
void parser_reset(void);
const char* parser_get_log(void);
int yyparse(void);

int main(void){
  parser_reset();
  yyparse();
  fputs(parser_get_log(), stdout);
  return 0;
}
