#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#include "symtab.h"
#include "helper.h"

entry_t* entry_create(symbol_t sym)
{
  entry_t* entry = (entry_t*)malloc(sizeof(entry_t));
  
  entry->sym = sym;
  entry->next = NULL;

  return entry;
}

symtab_t* symtab_create(symtab_t* encl)
{
  int i;
  symtab_t* symtab;
  symbol_t dummy_symbol = {NULL, 0};

  symtab = (symtab_t*)malloc(sizeof(symtab_t));
  symtab->encl = encl;
  
  for(i = 0; i < BUCKET_SIZE; i++) {
    symtab->bucket[i] = entry_create(dummy_symbol);
  }

  return symtab;
}

symtab_t* symtab_dup(symtab_t* tab)
{
  int i;
  symtab_t* dup;

  if(tab == NULL) return NULL;

  dup = symtab_create(NULL);
  for(i = 0; i < BUCKET_SIZE; i++) {
    entry_t* p = tab->bucket[i]->next;
    while(p != NULL) {
      symtab_enter(dup, p->sym, p->value);
      p = p->next;
    }
  }
  dup->encl = symtab_dup(tab->encl);

  return dup;
}

void symtab_enter(symtab_t* symtab, symbol_t sym, object_t* obj)
{
  int hash = symbol_hash(sym);
  entry_t* entry = symtab->bucket[hash];

  while(entry && entry->next != NULL) {
    if(symbol_equal(sym, entry->sym)) {
      entry->value = obj;
      break;
    }
    entry = entry->next;
  }

  if(entry->next == NULL) {
    if(symbol_equal(sym, entry->sym)) {
      entry->value = obj;
    }
    else {
      entry_t* new_entry = entry_create(sym);
      new_entry->value = obj;
      entry->next = new_entry;
    }
  }
}

int symbol_hash(symbol_t sym)
{
  return 10;
}

int symbol_equal(symbol_t a, symbol_t b)
{
  if(a.len == 0 || b.len == 0) {
    return 0;
  }
  else if((a.len == b.len) && (strncmp(a.p, b.p, a.len) == 0)) {
    return 1;
  }
  else {
    return 0;
  }
}

object_t* symtab_lookup(symtab_t* symtab, symbol_t sym)
{
  int hash = symbol_hash(sym);

  do {
    entry_t* entry = symtab->bucket[hash];

    while(entry != NULL) {
      if(symbol_equal(sym, entry->sym)) {
	return entry->value;
      }
      entry = entry->next;
    }

    symtab = symtab->encl;
  } while(symtab);
  
  return NULL;
}

