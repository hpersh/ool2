%{

#include <stdio.h>
#include <string.h>

#include "ool.h"

#define YYSTYPE  obj_t

extern char *yytext;
extern int  yyleng;

%}

%token TOK_QUOTE
%token TOK_LPAREN
%token TOK_COMMA
%token TOK_RPAREN
%token TOK_LSQBR
%token TOK_CEQUAL
%token TOK_EQUAL
%token TOK_RSQBR
%token TOK_LBRACE
%token TOK_RBRACE
%token TOK_FLOATNUM
%token TOK_DECNUM
%token TOK_HEXNUM
%token TOK_CSYM
%token TOK_SYM
%token TOK_DSYM
%token TOK_QSTR
%token TOK_SELN
%start inp

%%

decnum:
        TOK_DECNUM
{
  integer_val_t val = 0;
  char          *fmt;
  
  fmt = yyleng > 1 && yytext[0] == '0'
    ? INTEGER_SCANF_FMT_OCT : INTEGER_SCANF_FMT_DEC;

  sscanf(yytext, fmt, &val);
  m_integer_new(val);
  
  vm_push(0);                /* Just for keeping a reference while parsing */
  $$ = R0;
}
	;

hexnum:
        TOK_HEXNUM
{
  integer_val_t val = 0;
  
  sscanf(yytext, INTEGER_SCANF_FMT_HEX, &val);
  m_integer_new(val);
  
  vm_push(0);
  $$ = R0;
}
	;

floatnum:
	TOK_FLOATNUM
{
  float_val_t val = 0;
  
  sscanf(yytext, FLOAT_SCANF_FMT, &val);
  m_float_new(val);
  
  vm_push(0);
  $$ = R0;
}
	;

csym:
        TOK_CSYM
{
  m_string_new(1, yyleng, yytext);

  vm_push(0);
  $$ = R0;
}
	;

qstr:
        TOK_QSTR
{
  unsigned size = yyleng - 2, n;
  char     *p;
  
  for (p = yytext + 1, n = size; n; --n, ++p) {
    char c;
    
    if (*p != '\\' || n < 2)  continue;
    switch (p[1]) {
    case '"':
      c = '"';
      break;
    case 'n':
      c = '\n';
      break;
    case 'r':
      c = '\r';
      break;
    case 't':
      c = '\t';
      break;
    default:
      continue;
    }
    memmove(p, p + 1, n - 1);
    *p = c;
    --n;
    --size;
  }
  
  m_string_new(1, size, yytext + 1);
  m_cons(R0, NIL);
  m_method_call_new(consts.str.aquote, R0);
  
  vm_push(0);
  $$ = R0;
}
	;

sym:
        TOK_SYM
{
  m_string_new(1, yyleng, yytext);

  vm_push(0);
  $$ = R0;
}
	;

dsym:
	TOK_DSYM
{
  unsigned k, n;
  char     *p, *q;
  
  vm_push(1);
  
  for (k = 0, p = yytext;; p = q + 1, ++k) {
    q = index(p, '.');
    n = q ? q - p : strlen(p);
    
    m_string_new(1, n, p);
    
    if (k == 0) {
      vm_assign(1, R0);
      continue;
    }
    
    m_cons(R0, NIL);
    m_method_call_new(consts.str.aquote, R0);
    m_cons(R0, NIL);
    m_cons(R1, R0);
    m_method_call_new(consts.str.atc, R0);
    vm_assign(1, R0);
    
    if (q == 0)  break;
  }
  
  vm_assign(0, R1);
  
  vm_pop(1);
  
  vm_push(0);
  $$ = R0;
}
	;

seln:
        TOK_SELN
{
  m_string_new(1, yyleng, yytext);

  vm_push(0);
  $$ = R0;
}
	;

atom:
        decnum
{
  vm_assign(0, $1);
  
  vm_push(0);
  $$ = R0;
}
        | hexnum
{
  vm_assign(0, $1);
  
  vm_push(0);
  $$ = R0;
}
        | floatnum
{
  vm_assign(0, $1);
  
  vm_push(0);
  $$ = R0;
}
        | sym
{
  vm_assign(0, $1);
  
  vm_push(0);
  $$ = R0;
}
        | csym
{
  vm_assign(0, $1);
  
  vm_push(0);
  $$ = R0;
}
        | seln
{
  vm_assign(0, $1);
  
  vm_push(0);
  $$ = R0;
}
        | qstr
{
  vm_assign(0, $1);
  
  vm_push(0);
  $$ = R0;
}
        ;

pair:
	TOK_LPAREN expr TOK_COMMA expr TOK_RPAREN
{
  m_pair_new($2, $4);

  vm_push(0);
  $$ = R0;
}
	;

list_exprs_1:
        list_exprs_1 expr
{
  m_list_concat(&$1, $2);
  
  vm_assign(0, $1);
  
  vm_push(0);
  $$ = R0;
}
        | expr
{
  m_cons($1, NIL);

  vm_push(0);
  $$ = R0;
}
        ;

list:
        TOK_LPAREN list_exprs_1 TOK_RPAREN
{
  vm_assign(0, $2);
  
  vm_push(0);
  $$ = R0;
}
        | TOK_LPAREN TOK_RPAREN
{
  vm_assign(0, NIL);
  
  $$ = R0;
}
        ;

method_call_sel_and_args:
        method_call_sel_and_args seln expr
{
  obj_t p, q;
  
  m_cons($3, NIL);
  m_cons($2, R0);
  _list_concat(&$1, R0);
  vm_assign(0, $1);
  
  vm_push(0);
  $$ = R0;
}
        | seln expr
{
  m_cons($2, NIL);
  m_cons($1, R0);
  
  vm_push(0);
  $$ = R0;
}
        ;

method_call:
	dsym
{
  vm_assign(0, $1);
  
  vm_push(0);
  $$ = R0;
}
	| TOK_LSQBR sym TOK_CEQUAL expr TOK_RSQBR
{
  vm_push(1);

  m_cons($4, NIL);
  vm_assign(1, R0);
  m_cons($2, NIL);
  m_method_call_new(consts.str.aquote, R0);
  m_cons(R0, R1);
  m_cons(consts.str.environment, R0);
  m_method_call_new(consts.str.newc_putc, R0);

  vm_pop(1);
    
  vm_push(0);
  $$ = R0;
}
	| TOK_LSQBR sym TOK_EQUAL expr TOK_RSQBR
{
  vm_push(1);

  m_cons($4, NIL);
  vm_assign(1, R0);
  m_cons($2, NIL);
  m_method_call_new(consts.str.aquote, R0);
  m_cons(R0, R1);
  m_cons(consts.str.environment, R0);
  m_method_call_new(consts.str.atc_putc, R0);

  vm_pop(1);
    
  vm_push(0);
  $$ = R0;
}
	| TOK_LSQBR dsym TOK_EQUAL expr TOK_RSQBR
{
  m_list_concat(&METHOD_CALL($2)->args, $4);
  m_method_call_new(consts.str.atc_putc, METHOD_CALL($2)->args);

  vm_push(0);
  $$ = R0;
}
        | TOK_LSQBR expr sym TOK_RSQBR
{
  m_cons($2, NIL);
  m_method_call_new($3, R0);

  vm_push(0);
  $$ = R0;
}
        | TOK_LSQBR expr csym TOK_RSQBR
{
  m_cons($2, NIL);
  m_method_call_new($3, R0);

  vm_push(0);
  $$ = R0;
}
        | TOK_LSQBR expr method_call_sel_and_args TOK_RSQBR
{
  obj_t    p, *q, r;
  unsigned n, k;
  char     *s;

  m_cons($2, $3);

  vm_pushm(1, 2);
  
  vm_assign(1, R0);
  vm_assign(2, NIL);
  for (k = n = 0, q = &R2, p = R1; p; p = CDR(p), ++n) {
    if (n & 1) {
      k += string_len(CAR(p));
      continue;
    }

    m_cons(CAR(p), NIL);
    OBJ_ASSIGN(*q, R0);
    q = &CDR(R0);
  }

  vm_inst_alloc(consts.cl.string);
  inst_init(R0, k + 1);
  for (k = n = 0, s = STRING(R0)->data, p = R1; p; p = CDR(p), ++n) {
    if ((n & 1) == 0)  continue;

    r = CAR(p);
    strcpy(s, STRING(r)->data);
    s += string_len(r);
  }

  m_method_call_new(R0, R2);

  vm_popm(1, 2);

  vm_push(0);
  $$ = R0;
}
        ;

block_args_1:
        block_args_1 sym
{
  m_list_concat(&$1, $2);
  
  vm_assign(0, $1);
  
  vm_push(0);
  $$ = R0;
}
        | sym
{
  m_cons($1, NIL);
  
  vm_push(0);
  $$ = R0;
}
        ;

block_args:
        TOK_LPAREN block_args_1 TOK_RPAREN
{
  vm_assign(0, $2);
  
  vm_push(0);
  $$ = R0;
}
        | TOK_LPAREN TOK_RPAREN
{
  vm_assign(0, NIL);
  
  $$ = R0;
}
        ;

block_body_1:
        block_body_1 expr
{
  m_list_concat(&$1, $2);
  
  vm_assign(0, $1);
  
  vm_push(0);
  $$ = R0;
}
        | expr
{
  m_cons($1, NIL);
  
  vm_push(0);
  $$ = R0;
}
        ;

block_body:
        block_body_1
{
  vm_assign(0, $1);
  
  vm_push(0);
  $$ = R0;
}
        | /* empty */
{
  vm_assign(0, NIL);
  
  $$ = R0;
}
        ;

block:
        TOK_LBRACE block_args block_body TOK_RBRACE
{
  m_cons($2, $3);
  m_block_new(R0);
  
  vm_push(0);
  $$ = R0;
}
        ;

expr:
        atom
{
  vm_assign(0, $1);
  
  vm_push(0);
  $$ = R0;
}
        | pair
{
  vm_assign(0, $1);
  
  vm_push(0);
  $$ = R0;
}
        | list
{
  vm_assign(0, $1);
  
  vm_push(0);
  $$ = R0;
}
        | method_call
{
  vm_assign(0, $1);
  
  vm_push(0);
  $$ = R0;
}
        | block
{
  vm_assign(0, $1);
  
  vm_push(0);
  $$ = R0;
}
        | TOK_QUOTE expr
{
  m_cons($2, NIL);
  m_method_call_new(consts.str.aquote, R0);
  
  vm_push(0);
  $$ = R0;
}
        ;

inp:
        expr
{
  vm_assign(0, $1);
  
  vm_push(0);
  $$ = R0;
  
  YYACCEPT;
}

