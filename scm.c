#include <stdio.h>
#include <stdlib.h>

#include "object.h"
#include "symtab.h"
#include "helper.h"
#include "list.h"

#define DATA_STACK_SIZE 1024
#define ds_push(ss, obj) do{*(ss)->ds->pc=(obj);(ss)->ds->pc++;}while(0)
#define ds_pop(ss) (*(--(ss)->ds->pc))
#define ds_popn(ss,n) do{(ss)->ds->pc-=n;}while(0)
#define ds_peek(ss) (*((ss)->ds->pc-1))
#define ds_peekn(ss,n) (*((ss)->ds->pc-n))

typedef struct {
  object_t** pc;
  object_t* data[DATA_STACK_SIZE];
} data_stack_t;

typedef struct {
  data_stack_t* ds;
  symtab_t* symtab;
} scm_state_t;

data_stack_t* data_stack_create()
{
  data_stack_t* ds = (data_stack_t*)malloc(sizeof(data_stack_t));
  ds->pc = ds->data;
  return ds;
}

scm_state_t* scm_state_create() 
{
  scm_state_t* ss; 
  data_stack_t* ds; 

  ss = (scm_state_t*)malloc(sizeof(scm_state_t));
  ds = data_stack_create();

  ss->ds = ds;
  ss->symtab = symtab_create(NULL);

  return ss;
}

void eval_object(object_t* value) 
{
  printf("value\n");
}

#define D(name) {name, sizeof(name)-1}

symbol_t builtin_functions[] = {
  D("lambda"), D("if"), D("cond"), D("quote"), D("macro"),
  D("+"), D("-"), D("*"), D("/"), D(">"), D("="), D("<"), D("%"),
  D("and"), D("or"), D("not"),
  D("set"), D("car"), D("cdr"), D("cons"), 
  D("atom"), D("progn"),
  D("message"), D("p")
};

enum {
  BF_LAMBDA, BF_IF, BF_COND, BF_QUOTE, BF_MACRO,
  BF_ADD, BF_SUB, BF_MUL, BF_DIV, BF_GT, BF_EQ, BF_LT, BF_MOD,
  BF_AND, BF_OR, BF_NOT,
  BF_SET, BF_CAR, BF_CDR, BF_CONS,
  BF_ATOM, BF_PROGN,
  BF_MESSAGE, BF_P
};

symbol_t builtin_atoms[] = {
  D("t"), D("f"), D("nil")
};

enum {
  BA_TRUE, BA_FALSE, BA_NIL
};

int builtin_function(symbol_t sym)
{
  int i;
  for(i = 0; i < sizeof(builtin_functions)/sizeof(symbol_t); i++) {
    if(symbol_equal(sym, builtin_functions[i])) {
      return i;
    }
  }
  return -1;
}

int builtin_atom(symbol_t sym)
{
  int i;
  for(i = 0; i < sizeof(builtin_atoms)/sizeof(symbol_t); i++) {
    if(symbol_equal(sym, builtin_atoms[i])) {
      return i;
    }
  }
  return -1;
}

void eval_sexpr(sexpr_t* expr, scm_state_t* ss);

void eval_builtin_atom(int bf_sym, scm_state_t* ss)
{
  object_t *obj;

  switch(bf_sym) {
  case BA_TRUE:
    obj = new_boolean(1);
    ds_push(ss, obj);
    break;
  case BA_FALSE:
    obj = new_boolean(0);
    ds_push(ss, obj);
    break;
  case BA_NIL:
    obj = new_nil();
    ds_push(ss, obj);
    break;
  default:
    lerror("error: unknown builtin atom (%d)\n", bf_sym);
  }
}

void eval_lambda(list_sexpr_t* tl, scm_state_t* ss, int bf_sym)
{
  sexpr_t* args = tl->head;
  list_sexpr_t* body = tl->tail;
  lambda_t* lambda;

  if(args == NIL || body == LNIL) {
    lerror("error: lamdba-expr not complete\n");
  }

  lambda = cast_lambda(new_lambda());

  if(args->type == SEXPR_SYM) {
    entry_t* arg = entry_create(as_atom(args)->value.symbol);
    lambda->flag = LAMBDA_UNIVAR;
    lambda->args = arg;
  }
  else if(args->type == SEXPR_LIST) {
    list_sexpr_t* lp = as_list(args);

    if(lp->head == NIL) {
      lambda->args = NULL;
    }
    else {
      entry_t** tail = &lambda->args;

      while(lp != LNIL) {
	entry_t* arg;
	if(lp->head->type != SEXPR_SYM) {
	  lerror("error: lambda arguments should be symbol\n");
	}
	arg = entry_create(as_atom(lp->head)->value.symbol);
	*tail = arg;
	tail = &arg->next;

	if(lp->foot != NULL) { // dot notation
	  lambda->flag = LAMBDA_DOTVAR;
	  if(lp->foot->type != SEXPR_SYM) {
	    lerror("error: lambda arguments should be symbol( after '.' )\n");
	  }
	  arg = entry_create(as_atom(lp->foot)->value.symbol);
	  *tail = arg;
	  break;
	}

	lp = lp->tail;
      }
    }
  }
  else {
    lerror("error: form of the lambda-expr should be SYM or LIST\n");
  }

  /* save environment */
  lambda->env = symtab_dup(ss->symtab);
  if(bf_sym == BF_MACRO)
    lambda->type = OBJ_MACRO;
  lambda->body = body->head;
  ds_push(ss, OBJECT(lambda));
}

void eval_builtin(int bf_sym, list_sexpr_t* args, scm_state_t* ss)
{
  int reg0;
  object_t *obj, *obj0, *obj1, *val;
  list_t *list, *nlist;
  quote_t* quote;
  list_sexpr_t *lp, *llp;

  switch(bf_sym) {
  case BF_P:
  case BF_MESSAGE:
    lp = args;
    while(lp != LNIL) {
      eval_sexpr(lp->head, ss);
      obj = ds_pop(ss);
      print_object(obj);
      printf(" ");
      lp = lp->tail;
    }
    printf("\n");
    break;
  case BF_PROGN:
    lp = args;
    while(lp != LNIL && lp->tail != LNIL) {
      eval_sexpr(lp->head, ss);
      lp = lp->tail;
    }
    if(lp != LNIL) {
      eval_sexpr(lp->head, ss);
    }
    break;
  case BF_ATOM:
    lp = args;
    if(lp->head == NIL) {
      lerror("error: missing argument of 'atom'\n");
    }
    eval_sexpr(lp->head, ss);
    val = ds_pop(ss);
    obj = new_boolean(!IS_LIST(val));
    ds_push(ss, obj);
    break;
  case BF_CONS:
    lp = args;
    if(lp->head == NIL || lp->tail == LNIL) {
      lerror("error: missing arguments of 'cons'\n");
    }
    eval_sexpr(lp->head, ss);
    eval_sexpr(lp->tail->head, ss);
    obj = ds_pop(ss); // second
    val = ds_pop(ss); // first

    nlist = cast_list(new_list());
    nlist->head = val;
    if(IS_NIL(obj)) {
      nlist->tail = NULL;      
    }
    else if(!IS_LIST(obj) && !IS_NIL(obj)) {
      nlist->foot = obj;
    }
    else {
      list = cast_list(obj);
      nlist->tail = list;
    }
    ds_push(ss, OBJECT(nlist));
    break;
  case BF_CAR:
    lp = args;
    if(lp->head == NIL) {
      lerror("error: miss argument of 'car'\n");
    }
    eval_sexpr(lp->head, ss);
    obj = ds_pop(ss);
    list = cast_list(obj);
    if(list->head == NULL) {
      ds_push(ss, new_nil());
    }
    else if(!IS_LIST(obj)) {
      lerror("error: illegal argument of 'car'(%d)\n", obj->type);
    }
    else {
      ds_push(ss, list->head);
    }
    break;
  case BF_CDR:
    lp = args;
    if(lp->head == NIL) {
      lerror("error: miss argument of 'cdr'\n");
    }
    eval_sexpr(lp->head, ss);
    obj = ds_pop(ss);
    list = cast_list(obj);
    if(list->head == NULL) {
      lerror("error: cant 'cdr' on empty list:\t\n");
      println_object(obj);
    }
    else if(list->tail == NULL) {
      if(list->foot == NULL)
	ds_push(ss, new_nil());
      else
	ds_push(ss, OBJECT(list->foot));
    }
    else {
      ds_push(ss, OBJECT(list->tail));
    }
    break;
  case BF_SET:
    lp = args;
    if(lp->head == NIL || lp->tail == LNIL) {
      lerror("error: miss oprands of 'set'\n");
    }
    eval_sexpr(lp->head, ss);
    obj = ds_pop(ss);
    if(!IS_QUOTE(obj)) {
      lerror("error: first arg should be quote of 'set'\n");
    }
    quote = cast_quote(obj);
    
    /* add dumpy sym for recursion call */
    symtab_enter(ss->symtab, as_atom(quote->expr)->value.symbol, new_nil());

    eval_sexpr(lp->tail->head, ss);
    val = ds_pop(ss);
    if(quote->expr->type != SEXPR_SYM) {
      lerror("error: first arg should be quote of 'set'\n");
    }
    symtab_enter(ss->symtab, as_atom(quote->expr)->value.symbol, val);
    /* update closure environment for recursion call */
    if(IS_LAMBDA(val))
      symtab_enter(cast_lambda(val)->env, as_atom(quote->expr)->value.symbol, val);
    ds_push(ss, NULL); // ds_push(ss, val);
    break;
  case BF_GT:
    lp = args;
    if(lp->head == NIL || lp->tail == LNIL) {
      lerror("error: miss oprands of '>'\n");
    }
    eval_sexpr(lp->head, ss);
    eval_sexpr(lp->tail->head, ss);

    obj0 = ds_pop(ss);
    obj1 = ds_pop(ss);
    if(!IS_NUMBER(obj0) || !IS_NUMBER(obj1)) {
      lerror("error: oprands should be number of '>'\n");
    }
    obj = new_boolean(cast_number(obj1)->n > cast_number(obj0)->n);
    ds_push(ss, obj);
    break;
  case BF_LT:
    lp = args;
    if(lp->head == NIL || lp->tail == LNIL) {
      lerror("error: miss oprands of '>'\n");
    }
    eval_sexpr(lp->head, ss);
    eval_sexpr(lp->tail->head, ss);

    obj0 = ds_pop(ss);
    obj1 = ds_pop(ss);
    if(!IS_NUMBER(obj0) || !IS_NUMBER(obj1)) {
      lerror("error: oprands should be number of '<'\n");
    }
    obj = new_boolean(cast_number(obj1)->n < cast_number(obj0)->n);
    ds_push(ss, obj);
    break;
  case BF_MOD:
    lp = args;
    if(lp->head == NIL || lp->tail == LNIL) {
      lerror("error: miss oprands of '>'\n");
    }
    eval_sexpr(lp->head, ss);
    eval_sexpr(lp->tail->head, ss);

    obj0 = ds_pop(ss);
    obj1 = ds_pop(ss);
    if(!IS_NUMBER(obj0) || !IS_NUMBER(obj1)) {
      lerror("error: oprands should be number of '%'\n");
    }
    obj = new_number(cast_number(obj1)->n % cast_number(obj0)->n);
    ds_push(ss, obj);
    break;
  case BF_EQ:
    lp = args;
    if(lp->head == NIL || lp->tail == LNIL) {
      lerror("error: miss oprands of '='\n");
    }
    eval_sexpr(lp->head, ss);
    eval_sexpr(lp->tail->head, ss);

    obj0 = ds_pop(ss);
    obj1 = ds_pop(ss);

    reg0 = object_equal(obj0, obj1);
    if(reg0 < 0) {
      lerror("error: oprands not same type or not supported type of '='\n");
    }
    obj = new_boolean(reg0);
    ds_push(ss, obj);
    break;
  case BF_AND:
    lp = args;
    if(lp->head == NIL || lp->tail == LNIL) {
      lerror("error: miss oprands of 'and'\n");
    }
    eval_sexpr(lp->head, ss);
    eval_sexpr(lp->tail->head, ss);

    obj0 = ds_pop(ss);
    obj1 = ds_pop(ss);

    if((IS_BOOLEAN(obj0) || IS_NIL(obj0)) && 
       (IS_BOOLEAN(obj1) || IS_NIL(obj1))) {
      obj = new_boolean(cast_boolean(obj0)->val * cast_boolean(obj1)->val);
      ds_push(ss, obj);
    }
    else {
      lerror("error: wrong type of logic operation 'and'\n");
    }
    break;
  case BF_OR:
    lp = args;
    if(lp->head == NIL || lp->tail == LNIL) {
      lerror("error: miss oprands of 'or'\n");
    }
    eval_sexpr(lp->head, ss);
    eval_sexpr(lp->tail->head, ss);

    obj0 = ds_pop(ss);
    obj1 = ds_pop(ss);

    if((IS_BOOLEAN(obj0) || IS_NIL(obj0)) && 
       (IS_BOOLEAN(obj1) || IS_NIL(obj1))) {
      obj = new_boolean(cast_boolean(obj0)->val + cast_boolean(obj1)->val);
      ds_push(ss, obj);
    }
    else {
      lerror("error: wrong type of logic operation 'or'\n");
    }
    break;
  case BF_NOT:
    lp = args;
    if(lp->head == NIL) {
      lerror("error: miss oprands of 'not'\n");
    }
    eval_sexpr(lp->head, ss);

    obj0 = ds_pop(ss);

    if(IS_BOOLEAN(obj0) || IS_NIL(obj0)) {
      obj = new_boolean(!cast_boolean(obj0)->val);
      ds_push(ss, obj);
    }
    else {
      lerror("error: wrong type of logic operation 'not'\n");
    }
    break;
  case BF_LAMBDA:
  case BF_MACRO:
    eval_lambda(args, ss, bf_sym);
    break;
  case BF_IF:
    lp = args;
    eval_sexpr(lp->head, ss);
    obj = ds_pop(ss);
    if(!(IS_BOOLEAN(obj))) {
      lerror("error: first argument must be a boolean of 'if'\n");
    }
    if(lp->tail == LNIL || lp->tail->tail == LNIL) {
      lerror("error: miss oprands of 'if'\n");
    }
    if(IS_TRUE(obj)) {
      eval_sexpr(lp->tail->head, ss);
    }
    else {
      eval_sexpr(lp->tail->tail->head, ss);
    }
    break;
  case BF_COND:
    lp = args;
    while(lp && lp->head != NIL) {
      if(lp->head->type != SEXPR_LIST) {
	lerror("error: arguments of 'cond' should be LIST ");
      }
      llp = as_list(lp->head);
      eval_sexpr(llp->head, ss);
      obj = ds_pop(ss);
      if(IS_TRUE(obj)) {
	// eval value
	eval_sexpr(llp->tail->head, ss);
	obj = ds_peek(ss);
	break;
      }
      lp = lp->tail;
    }
    break;
  case BF_QUOTE:
    lp = args;
    obj = new_quote(as_sexpr(lp->head));
    ds_push(ss, obj);
    break;
  case BF_ADD:
    reg0 = 0;
    lp = args;
    while(lp != LNIL) {
      eval_sexpr(lp->head, ss);
      obj = ds_pop(ss);
      reg0 += cast_number(obj)->n;
      lp = lp->tail;
    }
    ds_push(ss, new_number(reg0));
    break;
  case BF_SUB:
    lp = args;
    if(lp->head != NIL) {
      eval_sexpr(lp->head, ss);
      obj = ds_pop(ss);
      reg0 = cast_number(obj)->n;

      lp = lp->tail;
      if(lp == LNIL) {
	lerror("error: miss 1 arg of '-' \n");
      }
      while(lp != LNIL) {
	eval_sexpr(lp->head, ss);
	obj = ds_pop(ss);
	reg0 -= cast_number(obj)->n;
	lp = lp->tail;
      }
      ds_push(ss, new_number(reg0));
    }
    else {
      lerror("error: miss 2 args of '-' \n");
    }
    break;
  case BF_MUL:
    reg0 = 1;
    lp = args;
    while(lp != LNIL) {
      eval_sexpr(lp->head, ss);
      obj = ds_pop(ss);
      reg0 *= cast_number(obj)->n;
      lp = lp->tail;
    }
    ds_push(ss, new_number(reg0));
    break;
  case BF_DIV:
    lp = args;
    if(lp->head != NIL) {
      eval_sexpr(lp->head, ss);
      obj = ds_pop(ss);
      reg0 = cast_number(obj)->n;

      lp = lp->tail;
      while(lp != LNIL) {
	eval_sexpr(lp->head, ss);
	obj = ds_pop(ss);
	reg0 /= cast_number(obj)->n;
	lp = lp->tail;
      }
      ds_push(ss, new_number(reg0));
    }
    else {
      lerror("error: miss args of '-' \n");
    }
    break;
  }
}

object_t* quote_sexpr_recur(sexpr_t* expr)
{
  if(expr == NIL) {
    return new_nil();
  }
  else if(expr->type == SEXPR_LIST) {
    list_t* head = NULL;
    list_t* last = NULL;
    list_sexpr_t* lst = NULL;

    lst = as_list(expr);

    while(lst != LNIL && lst->head != NIL) {
      list_t* lt = NULL;
      object_t* q = quote_sexpr_recur(lst->head);

      lt = cast_list(new_list());
      lt->head = q;
      lt->tail = NULL;
      if(last != NULL) {
	last->tail = lt;
      }
      if(head == NULL) {
	head = lt;
      }
      if(lst->foot != NIL) {
	object_t* foot = quote_sexpr_recur(lst->foot);
	lt->foot = foot;
      }
      last = lt;
      lst = lst->tail;
    }

    if(head == NULL)
      return new_list();
    else
      return OBJECT(head);
  }
  else {
    object_t* q = new_quote(expr);
    return q;
  }
}

sexpr_t* dequote_object_recur(object_t* obj)
{
  sexpr_t* expr;
  list_sexpr_t* sl;
  list_t* ll;

  if(obj == NULL) {
    return NULL;
  }
  else if(IS_LIST(obj)) {
    sl = list_create();
    ll = cast_list(obj);

    while(ll && ll->tail != NULL) {
      expr = dequote_object_recur(ll->head);
      list_append(sl, expr);
      ll = ll->tail;
    }
    expr = dequote_object_recur(ll->head);
    list_append(sl, expr);
    if(ll->foot) {
      expr = dequote_object_recur(ll->foot);
      list_set_foot(sl, expr);
    }
    return as_sexpr(sl);
  }
  else if(IS_QUOTE(obj)) {
    expr = cast_quote(obj)->expr;
    return expr;
  }
  else {
    lerror("error: unknown type while dequote macro(%d)\n", obj->type);
    return NULL;
  }
}

void macro_exec(object_t* obj, scm_state_t* ss)
{
  sexpr_t* expr = dequote_object_recur(obj);
#if DEBUG
  printf("*MACRO-EXPANSION*:\n  ");
  print_sexpr(expr);
  printf("\n");
#endif
  eval_sexpr(expr, ss);
}

void eval_apply(lambda_t* lambda, list_sexpr_t* args, scm_state_t* ss)
{
  int count, n;
  entry_t* arg;
  object_t* obj;
  symtab_t *new_env, *old_env;
  list_sexpr_t* p = args;

  arg = lambda->args;

  /* push args */
  count = 0;
  while(p != LNIL) {
    if(IS_LAMBDA(lambda)) {
      eval_sexpr(p->head, ss);
    }
    else {
      obj = quote_sexpr_recur(p->head);
      ds_push(ss, obj);
    }
    p = p->tail;
    count++;
  }

  old_env = ss->symtab;
  new_env = symtab_create(lambda->env);
  ss->symtab = new_env;

  if(lambda->flag == LAMBDA_UNIVAR) {
    list_t* head = NULL;
    list_t* lp = NULL;
    arg = lambda->args;
    while(count > 0) {
      obj = ds_pop(ss);
      head = cast_list(new_list());
      head->head = obj;
      head->tail = lp;
      lp = head;
      count--;
    }
    if(head == NULL)
      symtab_enter(new_env, arg->sym, OBJECT(new_nil()));
    else
      symtab_enter(new_env, arg->sym, OBJECT(head));
  }
  else if(lambda->flag == LAMBDA_DOTVAR) {
    int dot_len, m;
    list_t* dot_list = NULL;
    list_t* lp = NULL;

    /* push dot as list */
    n = 0;
    while(arg) {
      arg = arg->next;
      n++;
    }
    dot_len = count - n + 1;
    arg = lambda->args;
    while(dot_len > 0) {
      obj = ds_pop(ss);
      dot_list = cast_list(new_list());
      dot_list->head = obj;
      dot_list->tail = lp;
      lp = dot_list;
      dot_len--;
    }

    /* args before dot */
    n--;
    m = n;
    while(n > 0) {
      obj = ds_peekn(ss, n);
      symtab_enter(new_env, arg->sym, obj);
      arg = arg->next;
      n--;
    }
    ds_popn(ss, m);

    /* dot sym */
    if(dot_list == NULL)
      symtab_enter(new_env, arg->sym, OBJECT(new_nil()));
    else
      symtab_enter(new_env, arg->sym, OBJECT(dot_list));
  }
  else {
    n = 0;
    while(arg) {
      arg = arg->next;
      n++;
    }
    if(n != count) {
      print_sexpr(lambda->body);
      lerror("error: number of args of lambda error\n");
    }
    arg = lambda->args;

    /* argument substitude */
    while(count > 0) {
      obj = ds_peekn(ss, count);
      symtab_enter(new_env, arg->sym, obj);
      arg = arg->next;
      count--;
    }

    ds_popn(ss, n);
  }

  /* call function */
  eval_sexpr(lambda->body, ss);
  ss->symtab = old_env;

  if(IS_MACRO(lambda)) {
    obj = ds_pop(ss);
    macro_exec(obj, ss);
  }
}

void eval_list(sexpr_t* expr, scm_state_t* ss)
{
  sexpr_t* head;
  list_sexpr_t* list;
  object_t* obj;

  list = as_list(expr);
  head = list_head(list);

  if(head == NIL) {
    ds_push(ss, new_nil());
  }
  else if(head->type == SEXPR_SYM) {
    symbol_t sym = as_atom(head)->value.symbol;
    int bf_sym = builtin_function(sym);

    if(bf_sym >= 0) {
      eval_builtin(bf_sym, list->tail, ss);
    }
    else {
      obj = symtab_lookup(ss->symtab, sym);
      if(obj == NULL) {
	print_symbol(sym);
	lerror("error: symbol not found\n");
      }
      else if(IS_LAMBDA(obj) || IS_MACRO(obj)) {
	eval_apply(cast_lambda(obj), list->tail, ss);
      } 
      else {
	print_symbol(sym);
	lerror("error: the first element of list should be a function or macro: ");
      }
    }
  }
  else if(head->type == SEXPR_LIST) {
    /* get function */
    eval_sexpr(head, ss);
    obj = ds_pop(ss);
    if(IS_LAMBDA(obj) || IS_MACRO(obj)) {
      lambda_t* lambda;
      lambda = cast_lambda(obj);
      eval_apply(lambda, list->tail, ss);
    }
    else {
      lerror("error: the first element of list should be a function");
    }
  }
}

void eval_pair_notation(sexpr_t* expr, scm_state_t* ss)
{
  object_t *obj0, *obj1;
  list_t* list;
  list_sexpr_t* sl = as_list(expr);
  
  if(sl->head == NIL || sl->foot == NIL) {
    print_sexpr(expr);
    lerror("error: illegal use of '.' in");
  }

  eval_sexpr(sl->head, ss);
  eval_sexpr(sl->foot, ss);

  obj1 = ds_pop(ss);
  obj0 = ds_pop(ss);

  list = cast_list(new_list());
  list->head = obj0;
  list->foot = obj1;

  ds_push(ss, OBJECT(list));
}

void eval_sexpr(sexpr_t* expr, scm_state_t* ss)
{
  object_t* obj;
  int type = expr->type;

  if(type == SEXPR_SYM) {
    int bf_sym;
    atom_sexpr_t* atom;
    symbol_t sym;
    
    atom = as_atom(expr);
    sym = atom->value.symbol;
    bf_sym = builtin_atom(sym);

    if(bf_sym >= 0) {
      eval_builtin_atom(bf_sym, ss);
    }
    else {
      obj = symtab_lookup(ss->symtab, sym);
      if(obj) {
	ds_push(ss, obj);
      }
      else {
	print_sexpr(expr);
	lerror("error: unknown symbol '");
      }
    }
  }
  else if(type == SEXPR_NUM) {
    atom_sexpr_t* atom;
    atom = as_atom(expr);
    obj = new_number(atom->value.number);
    ds_push(ss, obj);
  }
  else if(type == SEXPR_LIST) {
    if(list_is_pair_notation(as_list(expr))) {
      eval_pair_notation(expr, ss);
    }
    else {
      eval_list(expr, ss);
    }
  }
  else if(type == SEXPR_QUOTE) {
    quote_sexpr_t* quote;
    quote = as_quote(expr);
    obj = new_quote(quote->expr);
    ds_push(ss, obj);
  }
}

void read_eval_loop(lexer_t* lexer)
{
  scm_state_t* ss = scm_state_create();
  ds_push(ss, NULL);

  while(token_stream_peek(lexer) > 0) {
    sexpr_t* expr = read_sexpr(lexer);
    eval_sexpr(expr, ss);
    if(ds_peek(ss))
      println_object(ds_peek(ss));
  }
}

void scm_main(char* filename) 
{
  lexer_t lexer;
  init_lexer(filename, &lexer);
  read_eval_loop(&lexer);
}

int main(int argc, char** argv)
{
  if(argc != 2){
    printf("Usage: cscm filename\n");
    exit(1);
  }
  scm_main(argv[1]);
  return 0;
}

