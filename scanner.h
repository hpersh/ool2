#include <stdio.h>

struct inp_desc {
  struct   inp_desc *prev;
  FILE     *fp;
  char     *filename;
  unsigned line;
  char     *str;
  void     *yybs_prev;
};

void yy_input_init(void);
void yy_input_push(struct inp_desc *p);
void yy_input_pop(struct inp_desc *p);
