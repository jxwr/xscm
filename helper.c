#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdarg.h>

#include "lexer.h"
#include "sexpr.h"
#include "object.h"

void lerror(char *format, ...)
{
  va_list args;
  va_start(args, format);
  vfprintf(stderr, format, args);
  va_end(args);
  exit(-1);
}

void print_symbol(symbol_t symbol) 
{
  char* p = symbol.p;
  int len = symbol.len;
  
  if(p != NULL && len > 0) {
    //    printf("<%p>", p);
    char ch = p[len];
    p[len] = 0;
    printf("%s", p);
    p[len] = ch;
  }
  else {
    printf("nil");
  }
}

void print_sexpr(sexpr_t* expr) 
{
  if(expr == NIL) {
    
  }
  else if(expr->type == SEXPR_NUM) {
    printf("%ld", ((atom_sexpr_t*)expr)->value.number);
  }
  else if(expr->type == SEXPR_SYM) {
    print_symbol(((atom_sexpr_t*)expr)->value.symbol);
  }
  else if(expr->type == SEXPR_LIST) {
    printf("(");
    list_sexpr_t* p = (list_sexpr_t*)expr;

    if(p != LNIL) {
      while(p->tail != LNIL) {
	print_sexpr(p->head);
	p = p->tail;
	printf(" ");
      }
      print_sexpr(p->head);
      if(p->foot != NIL) {
	printf(" . ");
	print_sexpr(p->foot);
      }
    }
    printf(")");
  }
  else if(expr->type == SEXPR_QUOTE) {
    quote_sexpr_t* quote = (quote_sexpr_t*)expr;
    
    printf("'");
    print_sexpr(quote->expr);
  }
}

void print_symtab(symtab_t* symtab)
{
  int i;
  for(i = 0; i < BUCKET_SIZE; i++) {
    entry_t* p = symtab->bucket[i];
    while(p) {
      if(p->value != 0) {
	print_symbol(p->sym);
	printf("->");
      }
      p = p->next;
    }
    //printf("\n");
  }
}
