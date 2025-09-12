#include <stdio.h>
#include <locale.h>

void parser_reset(void);
const char* parser_get_log(void);
const char* exec_get_output(void);
int parser_get_error_count(void);
int yyparse(void);

int main(void){
  setlocale(LC_ALL, "C");  /* <- importante para floats con '.' */
  parser_reset();
  yyparse();

  if (parser_get_error_count()==0) {
    const char* out = exec_get_output();
    if (out && *out) fputs(out, stdout);
  }
  fputs(parser_get_log(), stdout);
  return 0;
}
