%option noyywrap

%{

#include <stdlib.h>
#include <stdio.h>

#include "scanner.h"
#include "ool.h"

#include "grammar.tab.h"

struct inp_desc *inp_desc_cur;
YY_BUFFER_STATE yybs_cur;

void
yy_input_init(void)
{
  yybs_cur = yy_create_buffer(stdin, YY_BUF_SIZE);
  yy_switch_to_buffer(yybs_cur);
}

void
yy_input_push(struct inp_desc *p)
{
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
yy_input_pop(struct inp_desc *p)
{
  yy_delete_buffer(yybs_cur);

  yybs_cur = p->yybs_prev;
  yy_switch_to_buffer(yybs_cur);

  inp_desc_cur = p->prev;
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

"//" {
    char c;

    do {
        c = input();
    } while (!(c == '\n' || c == EOF));

#if 0
    if (yysp)  ++yysp->line;
#endif
}

"\n"	{ if (inp_desc_cur)  ++inp_desc_cur->line; }

"-"?{DECDIGIT}+"."{DECDIGIT}+	{ return (TOK_FLOATNUM); }

"-"?{DECDIGIT}+			{ return (TOK_DECNUM); }

0[Xx]{HEXDIGIT}+		{ return (TOK_HEXNUM); }

{CSYM}				{ return (TOK_CSYM); }

{SYM}				{ return (TOK_SYM); }

{DSYM}				{ return (TOK_DSYM); }

{DQUOTE}({EDQUOTE}|{NDQUOTE})*{DQUOTE}        { return (TOK_QSTR); }

{ALPHA}({ALPHA}|{DECDIGIT})*":"       { return (TOK_SELN); }

[ \t\r]       /* Skip whitespace */

.               { fprintf(stderr, "Bad character (0x%02x) in input\n", yytext[0]); }
