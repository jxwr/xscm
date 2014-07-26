#pragma once

#define DEBUG 0

#include "lexer.h"
#include "sexpr.h"

typedef struct {
  int type;
} object_t;

/* symbol table */

#define BUCKET_SIZE 31

typedef struct entry_s entry_t;
typedef struct symtab_s symtab_t;

struct symtab_s {
  entry_t* bucket[BUCKET_SIZE];
  struct symtab_s* encl;
};

struct entry_s {
  symbol_t sym;
  object_t* value;
  struct entry_s* next;
};

/* objects */

typedef struct list_s {
  int type;
  object_t* head;
  object_t* foot;
  struct list_s* tail;
} list_t;

typedef struct {
  int type;
  char* s;
} string_t;

typedef struct {
  int type;
  int n;
} number_t;

enum {
  LAMBDA_UNIVAR = 1, LAMBDA_DOTVAR
};

typedef struct {
  int type;
  int flag;
  symtab_t* env;
  entry_t* args;
  sexpr_t* body;
} lambda_t;

typedef struct {
  int type;
  sexpr_t* expr;
} quote_t;

typedef struct {
  int type;
  int val;
} boolean_t;

enum {
  OBJ_NUMBER, OBJ_LAMBDA, OBJ_LIST, OBJ_QUOTE, OBJ_BOOLEAN, OBJ_NIL, OBJ_MACRO
};

#define OBJECT(obj) ((object_t*)(obj))
#define IS_NUMBER(obj) ((obj)->type==OBJ_NUMBER)
#define IS_QUOTE(obj) ((obj)->type==OBJ_QUOTE)
#define IS_LIST(obj) ((obj)->type==OBJ_LIST)
#define IS_LAMBDA(obj) ((obj)->type==OBJ_LAMBDA)
#define IS_MACRO(obj) ((obj)->type==OBJ_MACRO)
#define IS_BOOLEAN(obj) ((obj)->type==OBJ_BOOLEAN)
#define IS_TRUE(obj) ((obj)->type==OBJ_BOOLEAN&&cast_boolean(obj)->val>0)
#define IS_FALSE(obj) ((obj)->type==OBJ_BOOLEAN&&cast_boolean(obj)->val<=0)
#define IS_NIL(obj) ((obj)->type==OBJ_NIL)

#define cast_number(obj) ((number_t*)(obj))
#define cast_lambda(obj) ((lambda_t*)(obj))
#define cast_list(obj) ((list_t*)(obj))
#define cast_quote(obj) ((quote_t*)(obj))
#define cast_boolean(obj) ((boolean_t*)(obj))

object_t* new_number(int val);
object_t* new_quote(sexpr_t* expr);
object_t* new_boolean(int bool);
object_t* new_lambda();
object_t* new_list();
object_t* new_nil();

int object_equal(object_t* obj1, object_t* obj0);

void print_object(object_t* obj);
void println_object(object_t* obj);








