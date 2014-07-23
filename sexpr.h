#pragma once

#include "lexer.h"

enum {
  SEXPR_SYM, SEXPR_NUM, SEXPR_LIST, SEXPR_QUOTE
};

typedef struct {
  int type;
} sexpr_t;

typedef struct list_sexpr_s {
  int type;
  sexpr_t* head;
  sexpr_t* foot; // totally wrong implementation
  struct list_sexpr_s* tail;
} list_sexpr_t;

typedef struct {
  int type;
  value_t value;
} atom_sexpr_t;

typedef struct {
  int type;
  sexpr_t* expr;
} quote_sexpr_t;

extern sexpr_t* NIL;
extern list_sexpr_t* LNIL;
extern const char* sexpr_type_names[];

sexpr_t* read_sexpr(lexer_t* lexer);

#define as_atom(expr) ((atom_sexpr_t*)(expr))
#define as_list(expr) ((list_sexpr_t*)(expr))
#define as_quote(expr) ((quote_sexpr_t*)(expr))
#define as_sexpr(expr) ((sexpr_t*)(expr))
