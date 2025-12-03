/* Filename: table.h
 * Transition Table and function declarations necessary for the scanner implementation 
 * as required for CST8152 - Assignment #2. Also contains many defined values
 * Version: 1.18.3
 * Date: 1 October 2018
 * Provided by: Svillen Ranev
 * Edit date: 7-NOV-2018
 * Completed by: Marco Gregory
 */

#ifndef  TABLE_H_
#define  TABLE_H_ 

#ifndef BUFFER_H_
#include "buffer.h"
#endif

#ifndef NULL
#include <_null.h> /* NULL pointer constant is defined there */
#endif

/*   Source end-of-file (SEOF) sentinel symbol
 *    '\0' or one of 255,0xFF,EOF
 */

/*  Special case tokens processed separately one by one
 *  in the token-driven part of the scanner
 *  '=' , ' ' , '(' , ')' , '{' , '}' , == , <> , '>' , '<' , ';',
 *  white space
 *  !!comment , ',' , ';' , '-' , '+' , '*' , '/', # ,
 *  .AND., .OR. , SEOF, 'illegal symbol',
 */

/*Special case: processed manually by switch. Whitespace not included*/
/*Arithmetic operators*/
#define C_EQUALS '='
#define C_PLUS '+'
#define C_MINUS '-'
#define C_DIV '/'
#define C_MULT '*'
/*Braces*/
#define BR_L '{'
#define BR_R '}'
#define PR_L '('
#define PR_R ')'
/*Relationals*/
#define C_GT '>'
#define C_LT '<'
/*other characters*/
#define C_DOT '.'
#define C_BANG '!'
#define C_SEOF '\0'
#define C_SEOF2 255
#define C_POUND '#'
#define C_CASH '$'
#define C_QUOTE '"'
#define C_SEMIC ';'
#define C_COMMA ','
#define ERR_STRING "RUN TIME ERROR: "
#define C_NL '\n'
#define C_CR '\r'

//REPLACE *ESN* and *ESR* WITH YOUR ERROR STATE NUMBER 
#define ES 11 /* Error state  with no retract */
#define ER 12 /* Error state  with retract */
#define IS -1 /* Invalid state */

/* State transition table definition */
#define TABLE_COLUMNS 8

/*transition table - type of states defined in separate table */
int  st_table[][TABLE_COLUMNS] = {
	//States are indexes. Table columns: [A-Za-z], 0, [1-9], ., $,	other, ", SEOF
	{1, 6, 4, ES, ES, ES, 9, ER}, /*0*/
	{1, 1, 1, 2, 3, 2, ES, ER}, /*1*/ /*SEOF was a 2 in my submission, should be ER. I apologize.*/
	{IS, IS, IS, IS, IS, IS, IS, IS}, /*2*/
	{IS, IS, IS, IS, IS, IS, IS, IS}, /*3*/
	{ES, 4, 4, 7, 5, 5, ES, ER}, /*4*/
	{IS, IS, IS, IS, IS, IS, IS, IS}, /*5*/
	{ES, 6, ES, 7, ES, 5, ES, ER}, /*6*/
	{8, 7, 7, 8, 8, 8, ES, ER}, /*7*/ /*changed dot ES to AS8 or 8..AND case*/
	{IS, IS, IS, IS, IS, IS, IS, IS},/*8*/
	{9, 9, 9, 9, 9, 9, 10, ER}, /*9*/
	{IS, IS, IS, IS, IS, IS, IS, IS}, /*10*/
	{IS, IS, IS, IS, IS, IS, IS, IS}, /*11*/
	{IS, IS, IS, IS, IS, IS, IS, IS} /*12*/
};
/* Accepting state table definition */
#define ASWR     3  /* accepting state with retract */
#define ASNR     4  /* accepting state with no retract */
#define NOAS     5  /* not accepting state */

int as_table[13] = {
	NOAS, /*1*/
	NOAS,
	ASWR,
	ASNR,
	NOAS,
	ASWR,
	NOAS,
	NOAS,
	ASWR,
	NOAS,
	ASNR, /*10*/
	ASNR,
	ASWR
};

/* Accepting action function declarations */

/*FOR EACH OF YOUR ACCEPTING STATES YOU MUST PROVIDE
ONE FUNCTION PROTOTYPE. THEY ALL RETURN Token AND TAKE
ONE ARGUMENT: A string REPRESENTING A TOKEN LEXEME. */
Token aa_func2(char *lexeme); 
Token aa_func3(char *lexeme);
Token aa_func5(char *lexeme);
Token aa_func6(char *lexeme);
Token aa_func7(char *lexeme);
Token aa_func8(char *lexeme);

/* defining a new type: pointer to function (of one char * argument) 
   returning Token
*/  
typedef Token (*PTR_AAF)(char *lexeme);
/* Accepting function (action) callback table (array) definition */
/* If you do not want to use the typedef, the equvalent declaration is:
 * Token (*aa_table[])(char lexeme[]) = {
 */

//array of pointers to functions that represent action for each state
PTR_AAF aa_table[] = {
	NULL,
	NULL,
	aa_func2, /*arithmetic variable identifier*/
	aa_func3, /*string variable identifier*/
	NULL,
	aa_func5, /*decimal integer literal*/
	NULL,
	NULL,
	aa_func6, /*floating point literal*/
	NULL,
	aa_func7, /*string literal*/
	aa_func8, /*error token*/
	aa_func8
};
/* Keyword lookup table (.AND. and .OR. are not keywords) */
#define KWT_SIZE  10

char * kw_table []=
	{
	"ELSE", /*0*/
	"FALSE", /*1*/
	"IF", /*2*/
	"PLATYPUS", /*3*/
	"READ", /*4*/
	"REPEAT",/*5*/
	"THEN",/*6*/
	"TRUE",/*7*/
	"WHILE",/*8*/
	"WRITE"  /*9*/
	};

#endif
                     