#ifndef PARSER_H_
#define PARSER_H _
#endif // !PARSER_H
 // !BUFFER_H_

#include "token.h"
#include "buffer.h"
#define ELSE 0
#define FALSE 1
#define IF 2
#define PLATYPUS 3
#define READ 4
#define REPEAT 5
#define THEN 6
#define TRUE 7
#define WHILE 8
#define WRITE 9


void print_token(Token t);


static Token lookahead;
int synerrno;

Token malar_next_token(void);
void program();
void syn_eh(int synccode);
void match(int tcode, int tattr);
void gen_incode(char* tmp);
void statements();
void statement();
void program();
void opt_statements();
void statementP();
void assignmentStatement();
void assignmentExpression();
void selectionStatement();
void preCondition();
void conditionalExpression();
void iterationStatement();
void inputStatement();
void variableIdentifier();
void variableList();
void variableListP();
void outputStatement();
void w_args();
void arithmeticExpression();
void unaryArithmeticExpression();
void primaryArithmeticExpression();
void additiveArithmeticExpression();
void additiveArithmeticExpressionP();
void multiplicativeArithmeticExpression();
void multiplicativeArithmeticExpressionP();
void stringExpression();
void primaryStringExpression();
void stringExpressionP();
void logORexpression();
void logANDexpression();
void logANDexpressionP();
void logORexpressionP();
void relationalExpression();
void primaryARelExp();
void primarySRelExp();
void primarySRelExpP();
void primaryARelExpP();
