#pragma once

enum token_t {
  TOK_NONE, TOK_OPEN, TOK_CLOSE, TOK_DOT, TOK_QUOTE, TOK_SYM, TOK_NUM
} ;

extern const char* token_names[];

typedef struct {
  char* p;
  int len;
} symbol_t;

typedef union {
  long number;
  symbol_t symbol;
} value_t;

typedef struct {
  char* cur;
  char* begin;
  char* end;
  int size;
  int line;
  int t;
  value_t value;
  int lookahead;
  value_t peek_value;
} lexer_t;

#define cur(lex) ((lex)->cur)
#define adv(lex) ((lex)->cur++)
#define eob(lex) ((lex)->cur>=(lex)->end)
#define curchar(lex) (*(lex)->cur)
#define symbol_ptr(lex) ((lex)->value.symbol.p)
#define symbol_len(lex) ((lex)->value.symbol.len)

int token_stream_next(lexer_t* lexer);
int token_stream_peek(lexer_t* lexer);
void init_lexer(const char* filename, lexer_t* lexer);

