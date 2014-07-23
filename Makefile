
src = helper.c lexer.c sexpr.c object.c symtab.c scm.c 
headers = helper.h lexer.h sexpr.h list.h object.h symtab.h

all: $(headlers)
	gcc $(src) -Wall -g -o scm

clean:
	-rm *~ scm

