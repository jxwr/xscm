#pragma once

#include "sexpr.h"

#define list_head(list) ((list)->head)
#define list_tail(list) ((list)->tail)

static inline 
void list_init(list_sexpr_t* list)
{
  list->type = SEXPR_LIST;
  list->head = NIL;
  list->foot = NIL;
  list->tail = LNIL;
}

static inline 
list_sexpr_t* list_create()
{
  list_sexpr_t* list = (list_sexpr_t*)malloc(sizeof(list_sexpr_t));
  list_init(list);
  return list;
}

static inline void list_set_head(list_sexpr_t* list, sexpr_t* head)
{
  list->head = head;
}

static inline 
void list_set_tail(list_sexpr_t* list, sexpr_t* elem)
{
  list_sexpr_t* new_list = list_create();
  list_set_head(new_list, elem);
  list->tail = new_list;
}

static inline
list_sexpr_t* list_get_last(list_sexpr_t* list) 
{
  list_sexpr_t* p = list;
  while(p != LNIL && p->tail != LNIL) {
    p = p->tail;
  }
  return p;
}

static inline
int list_is_pair_notation(list_sexpr_t* list)
{
  return (list_get_last(list)->foot != NULL);
}

static inline 
void list_set_foot(list_sexpr_t* list, sexpr_t* elem)
{
  list_get_last(list)->foot = elem;
}

static inline 
void list_append(list_sexpr_t* list, sexpr_t* elem)
{
  if(list_head(list) == NIL) {
    list_set_head(list, elem);
  }
  else if(list_tail(list) == LNIL) {
    list_set_tail(list, elem);
  }
  else {
    list_sexpr_t* p = list->tail;
    while(p->tail != LNIL) p = p->tail;
    list_set_tail(p, elem);
  }
}

