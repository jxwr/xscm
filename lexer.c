#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>
#include <math.h>

#include "lexer.h"
#include "helper.h"

const char* token_names[] = {
  "none", "(", ")", ".", "'", "symbol", "number"
};

static int symchar(int c)
{
  static char* special = "()';\\|";
  return (!isspace(c) && !strchr(special, c));
}

int token_stream_peek(lexer_t* lexer) 
{
  int ch;

  if(lexer->lookahead != TOK_NONE) {
    return lexer->lookahead;
  }

  do {
    if(eob(lexer)) {
      lexer->lookahead = TOK_NONE;
      return -1;
    }
    
    if(curchar(lexer) == ';') {
      do {
	adv(lexer);
      } while(curchar(lexer) != '\n' && !eob(lexer));
    }

    /* terminate condition */
    ch = curchar(lexer);
    if(!isspace(ch) && ch != ';') break;

    if(ch == '\n') lexer->line++;
    adv(lexer);
  } while(1);

  ch = curchar(lexer);
  if(eob(lexer)) {
    lexer->lookahead = TOK_NONE;
    return -1;
  }
  else if(ch == '(') {
    adv(lexer);
    lexer->lookahead = TOK_OPEN;
  }
  else if(ch == ')') {
    adv(lexer);
    lexer->lookahead = TOK_CLOSE;
  }
  else if(ch == '\'') {
    adv(lexer);
    lexer->lookahead = TOK_QUOTE;
  }
  else if(ch == '.') {
    adv(lexer);
    lexer->lookahead = TOK_DOT;
  }
  else if(ch == '-' || isdigit(ch)) {
    char* end;
    long val;

    val = strtol(lexer->cur, &end, 10);
    if(end > lexer->cur) {
      lexer->peek_value.number = val;
      lexer->lookahead = TOK_NUM;
      lexer->cur = end;
    }
    else {
      lexer->peek_value.symbol.p = lexer->cur;
      lexer->peek_value.symbol.len = 1;
      lexer->lookahead = TOK_SYM;
      adv(lexer);
    }
  }
  else if(symchar(ch)) {
    char* p = lexer->cur;
    while(symchar(*p)) p++;
    lexer->peek_value.symbol.p = lexer->cur;
    lexer->peek_value.symbol.len = p - lexer->cur;
    lexer->lookahead = TOK_SYM;
    lexer->cur = p;
  }
  else if(ch == '|') {
    char* p = lexer->cur+1;
    while(*p != '|' && p < lexer->end) p++;
    lexer->peek_value.symbol.p = lexer->cur + 1;
    lexer->peek_value.symbol.len = p - lexer->cur - 1;
    //printf("(len:%d)", symbol_len(lexer));
    lexer->lookahead = TOK_SYM;
    lexer->cur = p+1;
  }
  else {
    lerror("ERROR: unexpected input char '%c'(0x%x) at line: %d\n", 
	   ch, (unsigned int)ch,lexer->line);
  }

  //  printf("[%s]\n", token_names[lexer->lookahead]);
  return lexer->lookahead;
}

int token_stream_next(lexer_t* lexer)
{
  if(token_stream_peek(lexer) < 0) {
    return -1;
  }

  lexer->t = lexer->lookahead;
  lexer->lookahead = TOK_NONE;
  lexer->value = lexer->peek_value;

  return lexer->t;
}
    
void init_lexer(const char* filename, lexer_t* lexer) 
{
  char* buffer = NULL;
  FILE* file = NULL;
  long filesize, copysize;

  file = fopen(filename, "rb");
  if(file == NULL){
    fprintf(stderr, "error: can`t open file %s", filename);
    exit(1);
  }
  fseek(file, 0, 2);
  filesize = ftell(file);
  rewind(file);

  buffer = (char*)malloc(filesize + 1);
  if(buffer == NULL){
    fprintf(stderr, "No more memory");
    exit(2);
  }
  copysize = fread(buffer, 1, filesize, file);
  buffer[filesize] = 0;
  if(copysize != filesize){
    fprintf(stderr, "Not read the whole file");
    exit(3);
  }
  
  lexer->cur = buffer;
  lexer->begin = buffer, 
    lexer->end = buffer+filesize, 
    lexer->size = filesize,
    lexer->line = 1,
    lexer->t = TOK_NONE,
    lexer->lookahead = TOK_NONE;
}

/* test */
void test_lexer(lexer_t* lexer) {

  while(token_stream_next(lexer) > 0) {
    assert(token_stream_peek(lexer) == token_stream_peek(lexer));
  
    if(lexer->t == TOK_NUM) {
      printf("%ld ", lexer->value.number);
    }
    else if(lexer->t == TOK_SYM) {
      char* p = symbol_ptr(lexer);
      int len = symbol_len(lexer);
      char ch = p[len];

      p[len] = 0;
      printf("%s ", p);
      p[len] = ch;
    }
    else {
      printf("%s ", token_names[lexer->t]);
    }
  }
}
