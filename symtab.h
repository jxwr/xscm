#pragma once

#include "lexer.h"
#include "sexpr.h"
#include "object.h"

entry_t* entry_create(symbol_t sym);
symtab_t* symtab_create(symtab_t* encl);
symtab_t* symtab_dup(symtab_t* tab);
void symtab_enter(symtab_t* symtab, symbol_t sym, object_t* obj);
int symbol_hash(symbol_t sym);
int symbol_equal(symbol_t a, symbol_t b);
object_t* symtab_lookup(symtab_t* symtab, symbol_t sym);
