#include <stdio.h>
#include <stdlib.h>

#include "lexer.h"
#include "sexpr.h"
#include "helper.h"
#include "list.h"

sexpr_t* NIL = 0;
list_sexpr_t* LNIL = 0;

const char* sexpr_type_names[] = {
  "sexpr_sym", "sexpr_num", "sexpr_list", "sexpr_quote"
};

sexpr_t* read_symbol(value_t value)
{
  atom_sexpr_t* atom = (atom_sexpr_t*)malloc(sizeof(atom_sexpr_t));

  atom->type = SEXPR_SYM;
  atom->value = value;
  return (sexpr_t*)atom;
}

sexpr_t* read_number(value_t value)
{
  atom_sexpr_t* atom = (atom_sexpr_t*)malloc(sizeof(atom_sexpr_t));
  atom->type = SEXPR_NUM;
  atom->value = value;
  return as_sexpr(atom);
}

sexpr_t* read_sexpr(lexer_t* lexer);

sexpr_t* read_list(lexer_t* lexer)
{
  list_sexpr_t* list;
  sexpr_t* expr;
  int token;

  list = list_create();
  token_stream_next(lexer); /* skip '(' */

  token = token_stream_peek(lexer);
  while(token > 0 && token != TOK_CLOSE) {
    expr = read_sexpr(lexer);
    list_append(list, expr);
    token = token_stream_peek(lexer);

    if(token == TOK_DOT) {
      token_stream_next(lexer); /* skip '.' */
      expr = read_sexpr(lexer);
      list_set_foot(list, expr);
      token = token_stream_peek(lexer);
      break;
    }
  }

  if(token == TOK_CLOSE) {
    token_stream_next(lexer); /* skip ')' */
  }
  if(token < 0) {
    lerror("error: unexpected end of buffer\n");
  }

  return (sexpr_t*)list;
}

sexpr_t* read_quote(lexer_t* lexer)
{
  sexpr_t* expr;
  expr = read_sexpr(lexer);

  if(expr->type != SEXPR_NUM && expr->type != SEXPR_QUOTE) {
      quote_sexpr_t* quote;
      quote = (quote_sexpr_t*)malloc(sizeof(quote_sexpr_t));
      quote->type = SEXPR_QUOTE;
      quote->expr = expr;
      expr = as_sexpr(quote);
  }

  return expr;
}

sexpr_t* read_sexpr(lexer_t* lexer)
{
  sexpr_t* expr = NIL;

  //  printf("<%s>", token_names[token_stream_peek(lexer)]);
  
  switch(token_stream_peek(lexer)) {
  case TOK_CLOSE:
    token_stream_next(lexer);
    lerror("error: unexpected ')'\n");
  case TOK_DOT:
    token_stream_next(lexer);
    lerror("error: unexpected '.'\n");
  case TOK_SYM:
    token_stream_next(lexer);
    expr =read_symbol(lexer->value);
    break;
  case TOK_NUM:
    token_stream_next(lexer);
    expr = read_number(lexer->value);
    break;
  case TOK_QUOTE:
    token_stream_next(lexer);
    expr = read_quote(lexer);
    break;
  case TOK_OPEN:
    expr = read_list(lexer);
    break;
  }

  return expr;
}


