#include <stdio.h>

struct inp_desc {
  struct   inp_desc *prev;
  FILE     *fp;
  char     *filename;
  unsigned eof;
  unsigned line;
  char     *str;
  void     *yybs_prev;
};

void yy_input_init(void);
void yy_input_push(struct inp_desc *p);
void yy_input_pop(struct inp_desc *p);
void yy_input_flush(void);
unsigned yy_input_eof(void);
