#pragma once

#include "sexpr.h"

struct symtab_s;
typedef struct symtab_s symtab_t;

void lerror(char *format, ...);
void print_sexpr(sexpr_t* expr);
void print_symbol(symbol_t symbol);
void print_symtab(symtab_t* symtab);
