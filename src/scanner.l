%option noyywrap

%{

#include <stdlib.h>
#include <stdio.h>

#include "scanner.h"
#include "ool.h"

#include "grammar.tab.h"

struct inp_desc *inp_desc_cur;
YY_BUFFER_STATE yybs_cur;

struct inp_desc top_inp_desc[1];

void
yy_input_push(struct inp_desc *p)
{
  p->eof  = 0;
  p->line = 1;

  p->yybs_prev = yybs_cur;
  if (p->fp) {
    yybs_cur = yy_create_buffer(p->fp, YY_BUF_SIZE);
    yy_switch_to_buffer(yybs_cur);
  } else {
    yybs_cur = yy_scan_string(p->str);
  }

  p->prev = inp_desc_cur;
  inp_desc_cur = p;
}

void
yy_input_flush(void)
{
  yy_flush_buffer(yybs_cur);
}

void
yy_input_pop(struct inp_desc *p)
{
  yy_delete_buffer(yybs_cur);

  yybs_cur = p->yybs_prev;
  yy_switch_to_buffer(yybs_cur);

  inp_desc_cur = p->prev;
}

unsigned
yy_input_eof(void)
{
  return (inp_desc_cur->eof);
}

void
yy_input_init(void)
{
  top_inp_desc->fp = stdin;

  yy_input_push(top_inp_desc);
}

%}

ALPHA           [A-Za-z_\-]

DECDIGIT        [0-9]

HEXDIGIT        [0-9A-Fa-f]

DQUOTE          "\""

EDQUOTE         "\\\""

NDQUOTE         [^\"]

SYM		{ALPHA}({ALPHA}|{DECDIGIT})*

CSYM		"#"{SYM}

DSYM		({SYM}|{CSYM})("."({SYM}|{CSYM}))+


%%

<<EOF>> { inp_desc_cur->eof = 1;  return (TOK_EOF); }

"//" {
    char c;

    do {
        c = input();
    } while (!(c == '\n' || c == EOF));

    if (inp_desc_cur)  ++inp_desc_cur->line;
}

"\n"	{ if (inp_desc_cur)  ++inp_desc_cur->line; }

"-"?{DECDIGIT}+"."{DECDIGIT}+	{ return (TOK_FLOATNUM); }

"-"?{DECDIGIT}+			{ return (TOK_DECNUM); }

0[Xx]{HEXDIGIT}+		{ return (TOK_HEXNUM); }

{CSYM}				{ return (TOK_CSYM); }

"&"{0,2}{SYM}			{ return (TOK_SYM); }

{DSYM}				{ return (TOK_DSYM); }

{DQUOTE}({EDQUOTE}|{NDQUOTE})*{DQUOTE}        { return (TOK_QSTR); }

"&"{0,2}{ALPHA}({ALPHA}|{DECDIGIT})*":"       { return (TOK_SELN); }

"'"     { return (TOK_QUOTE); }

"("     { return (TOK_LPAREN); }

","     { return (TOK_COMMA); }

")"     { return (TOK_RPAREN); }

"["     { return (TOK_LSQBR); }

"="	{ return (TOK_EQUAL); }

":="	{ return (TOK_CEQUAL); }

"]"     { return (TOK_RSQBR); }

"{"     { return (TOK_LBRACE); }

"}"     { return (TOK_RBRACE); }

[ \t\r]       /* Skip whitespace */

.               { fprintf(stderr, "Bad character (0x%02x) in input\n", yytext[0]); }
