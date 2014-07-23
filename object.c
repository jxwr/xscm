#include <stdlib.h>
#include <stdio.h>

#include "sexpr.h"
#include "object.h"
#include "helper.h"

object_t* new_number(int val)
{
  number_t* obj = (number_t*)malloc(sizeof(number_t));
  obj->type = OBJ_NUMBER;
  obj->n = val;
  return OBJECT(obj);
}

object_t* new_quote(sexpr_t* expr)
{
  quote_t* obj = (quote_t*)malloc(sizeof(quote_t));
  obj->type = OBJ_QUOTE;
  obj->expr = expr;
  return OBJECT(obj);
}

object_t* new_boolean(int bool)
{
  boolean_t* obj = (boolean_t*)malloc(sizeof(boolean_t));
  obj->type = OBJ_BOOLEAN;
  obj->val = (bool > 0);
  return OBJECT(obj);
}

object_t* new_nil()
{
  boolean_t* obj = (boolean_t*)malloc(sizeof(boolean_t));
  obj->type = OBJ_NIL;
  return OBJECT(obj);
}

object_t* new_list()
{
  list_t* obj = (list_t*)malloc(sizeof(list_t));
  obj->type = OBJ_LIST;
  obj->head = NULL;
  obj->foot = NULL;
  obj->tail = NULL;
  return OBJECT(obj);
}

object_t* new_lambda()
{
  lambda_t* obj = (lambda_t*)malloc(sizeof(lambda_t));
  obj->type = OBJ_LAMBDA;
  obj->flag = -1;
  return OBJECT(obj);
}

int object_equal(object_t* obj1, object_t* obj0)
{
  if(IS_NIL(obj1) || IS_NIL(obj0)) {
    return IS_NIL(obj1) && IS_NIL(obj0);
  }
  if(obj0->type != obj1->type) {
    return -1;
  }
  if(IS_NUMBER(obj0)) {
    return (cast_number(obj1)->n == cast_number(obj0)->n);
  }
  return -1;
}

void print_object(object_t* obj)
{
  if(obj == NULL) {
    return;
  }
  else if(IS_NUMBER(obj)) {
    printf("%d", cast_number(obj)->n);
  }
  else if(IS_QUOTE(obj)) {
    //printf("(quote ");
    printf("'");
    print_sexpr(cast_quote(obj)->expr);
    //printf(")");
  }
  else if(IS_BOOLEAN(obj)) {
    printf("%s", cast_boolean(obj)->val > 0 ? "true" : "false");
  }
  else if(IS_LAMBDA(obj)) {
    printf("#<fn:%p>", obj);
  }
  else if(IS_MACRO(obj)) {
    printf("#<macro:%p>", obj);
  }
  else if(IS_NIL(obj)) {
    printf("nil");
  }
  else if(IS_LIST(obj)) {
    list_t* p = cast_list(obj);
    
    printf("(");
    if(p != NULL) {
      while(p->tail != NULL) {
	print_object(p->head);
	p = p->tail;
	printf(" ");
      }
      print_object(p->head);
      if(p->foot != NULL && !IS_NIL(p->foot)) {
	printf(" . ");
	print_object(p->foot);
      }
    }
    printf(")");
  }
  else {
    printf("error; unknown object <%d>", obj->type);
  }
}

void println_object(object_t* obj)
{
  print_object(obj);
  printf("\n");
}
