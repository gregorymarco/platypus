/********************************************************************************
Filename: scanner.c
List of Source/Headers: buffer.c, buffer.h, table.h, token.h, scanner.c

Compiler: MS VISUAL STUDIO 2017
Author: Marco Gregory	Student Number: 040876221
Course: Compilers 18F_CST8152_010
Assignment: TWO
Date: 07-NOV-2018
Professor: Svillen Ranev
Purpose: Used to produce a stream of platypus tokens (defined in token.h) from a file loaded into Buffer* psc_buf. Initialized with scanner_init(Buffer* psc_buf).
Function List: 
int scanner_init(Buffer * psc_buf); initializier
static int char_class(char c); character class function 
static int get_next_state(int, char, int *); state machine function 
static int getKWTPosition(char * kw_lexeme); keywords lookup function
Token malar_next_token(void); Gives tokens
Token aa_func2(char * lexeme) AVID acceptance
Token aa_func3(char * lexeme) SVID acceptance
Token aa_func5(char * lexeme) IL acceptance
Token aa_func6(char * lexeme) FPL acceptance
Token aa_func7(char * lexeme) SL acceptance
Token aa_func8(char * lexeme) ERR acceptance
********************************************************************************/
#define _CRT_SECURE_NO_WARNINGS

#include <stdio.h>   /* standard input / output */
#include <ctype.h>   /* conversion functions */
#include <stdlib.h>  /* standard library functions and constants */
#include <string.h>  /* string functions */
#include <limits.h>  /* integer types constants */
#include <float.h>   /* floating-point types constants */

#define NDEBUG       /*to suppress assert() call*/
#include <assert.h>  /* assert() prototype */

/* project header files */
/*#ifndef BUFFER_H_
#include "buffer.h"
#endif // !BUFFER_H_
#ifndef TOKEN_H_
#include "token.h"
#endif // !
#ifdef TABLE_H_
#include "table.h"
#endif // TABLE_H_/*/

#include "buffer.h"
#include "token.h"
#include "table.h"

#define DEBUG  /* for conditional processing */
#undef  DEBUG

/* Global objects - variables */
/* This buffer is used as a repository for string literals.*/
extern Buffer * str_LTBL; /*String literal table */
int line; /* current line number of the source code */
extern int scerrnum;     /* run-time error number */

/* Local(file) global objects - variables */
static Buffer *lex_buf;/*pointer to temporary lexeme buffer*/
static pBuffer sc_buf; /*pointer to input source buffer*/
/* No other global variable declarations/definitiond are allowed */

/* scanner.c static(local) function  prototypes */
static int char_class(char c); /* character class function */
static int get_next_state(int, char, int *); /* state machine function */
static int getKWTPosition(char * kw_lexeme); /*keywords lookup functuion */

/*Initializes scanner, provided by prof.*/
int scanner_init(Buffer * psc_buf) {
	if (b_isempty(psc_buf)) return EXIT_FAILURE;/*1*/
	/* in case the buffer has been read previously  */
	b_rewind(psc_buf);
	b_clear(str_LTBL);
	line = 1;
	sc_buf = psc_buf;
	return EXIT_SUCCESS;/*0*/
/*   scerrnum = 0;  *//*no need - global ANSI C */
}

static void print_token_internal(Token t) {
	extern char * kw_table[]; /* link to keyword table in */
	switch (t.code) {
	case  RTE_T:
		printf("RTE_T\t\t%s", t.attribute.err_lex);
		/*Call here run-time error handling component*/
		if (scerrnum) {
			printf("%d", scerrnum);
			exit(scerrnum);
		}printf("\n");
		break;
	case  ERR_T:
		printf("ERR_T\t\t%s\n", t.attribute.err_lex);
		break;
	case  SEOF_T:
		/*Call here buffer-full handling component*/
		printf("SEOF_T\t\t%d\t\n", t.attribute.seof);
		break;
	case  AVID_T:
		printf("AVID_T\t\t%s\n", t.attribute.vid_lex);
		break;
	case  SVID_T:
		printf("SVID_T\t\t%s\n", t.attribute.vid_lex);
		break;
	case  FPL_T:
		printf("FPL_T\t\t%f\n", t.attribute.flt_value);
		break;
	case  INL_T:
		printf("INL_T\t\t%d\n", t.attribute.get_int);
		break;
	case  STR_T:
		printf("STR_T\t\t%d\t ", (short)t.attribute.get_int);
		printf("%s\n", b_location(str_LTBL, (short)t.attribute.get_int));
		break;
	case  SCC_OP_T:
		printf("SCC_OP_T\n");
		break;
	case  ASS_OP_T:
		printf("ASS_OP_T\n");
		break;
	case  ART_OP_T:
		printf("ART_OP_T\t%d\n", t.attribute.get_int);
		break;
	case  REL_OP_T:
		printf("REL_OP_T\t%d\n", t.attribute.get_int);
		break;
	case  LOG_OP_T:
		printf("LOG_OP_T\t%d\n", t.attribute.get_int);
		break;
	case  LPR_T:
		printf("LPR_T\n");
		break;
	case  RPR_T:
		printf("RPR_T\n");
		break;
	case LBR_T:
		printf("LBR_T\n");
		break;
	case RBR_T:
		printf("RBR_T\n");
		break;
	case KW_T:
		printf("KW_T\t\t%s\n", kw_table[t.attribute.get_int]);
		break;
	case COM_T:
		printf("COM_T\n");
		break;
	case EOS_T:
		printf("EOS_T\n");
		break;
	case 20:
		printf("NOATTR");
	default:
		printf("Scanner error: invalid token code: %d\n", t.code);
	}
}
/********************************************************
Purpose: Match a lexeme by reading character by character from buffer. When a lexeme is identified, builds and returns a token with a valid
Token Code and Token Attribute. Ignores whitespace.
Author: Marco Gregory
History/Versions: 1.0
Called Functions: isKeyword, b_getc, b_retract, b_getcoffset, b_mark, get_next_state, b_reset, b_free, b_allocate 
Parameters: 
Return Value: Token t: see token.h. Token may or may not have attribute, but code will always be set
Algorithm:
	1: get first character from buffer
	2: check if it is a special case: if so, make token and return
	3: if not, begin matching lexeme and setting state using transition table
	4: get return token from accepting action function based on state and lexeme created
	5: return token
*********************************************************/
Token malar_next_token(void)
{
	Token t = { 0 }; /* token to return after pattern recognition. Set all structure members to 0 */
	unsigned char c; /* input symbol */
	int state = 0; /* initial state of the FSM */
	short lexstart;  /*start offset of a lexeme in the input char buffer (array) */
	short lexend;    /*end   offset of a lexeme in the input char buffer (array)*/
	int accept = NOAS; /* type of state - initially not accepting */
	/*DECLARE YOUR LOCAL VARIABLES HERE IF NEEDED   */
	char temp = 0; /*used as generic container for most operations*/

	while (1) { /* endless loop broken by token returns it will generate a warning */
		c = b_getc(sc_buf);
		/*Manual cases: Comment, whitespace, braces, arithmetic operators*/
		switch (c) {
		/*Comments*/
		case C_BANG:
			temp = b_getc(sc_buf); //next
			if (temp == '!') {
				/*while (b_getc(sc_buf) != C_NL);*/
				while (temp != C_NL && temp != C_CR && temp != C_SEOF && temp != C_SEOF2) { temp = b_getc(sc_buf); };
				if (temp == C_SEOF || temp == C_SEOF2) {
					t.code = ERR_T;
					t.attribute.err_lex[0] = c;
					t.attribute.err_lex[1] = C_SEOF;
					return t;
				}
				line += 1;
				continue;
			}
			else { /*invalid use of */
				t.code = ERR_T;
				t.attribute.err_lex[0] = c;
				t.attribute.err_lex[1] = temp;
				/*while (b_getc(sc_buf) != C_NL);*/
				while (temp != C_NL && temp != C_CR && temp != C_SEOF && temp != C_SEOF2) { temp = b_getc(sc_buf); };
				if (temp == C_SEOF) {
					t.code = SEOF_T;
					t.attribute.seof = SEOF2;
					return t;
				}
				line += 1;
				return t;
			}
			/*Whitespace*/
		case ' ':
			continue;
		case C_CR:
		case C_NL:
			line += 1;
			continue;
		case '\t':
			continue;
			/*braces and comma*/
		case PR_L:
			t.code = LPR_T;
			return t;
		case C_COMMA:
			t.code = COM_T;
			return t;
		case BR_L:
			t.code = LBR_T;
			return t;
		case PR_R:
			t.code = RPR_T;
			return t;
		case BR_R:
			t.code = RBR_T;
			return t;
			/*arithmetic operators*/
		case C_PLUS:
			t.code = ART_OP_T;
			t.attribute.arr_op = PLUS;
			return t;
		case C_MINUS:
			t.code = ART_OP_T;
			t.attribute.arr_op = MINUS;
			return t;
		case C_MULT:
			t.code = ART_OP_T;
			t.attribute.arr_op = MULT;
			return t;
		case C_DIV:
			t.code = ART_OP_T;
			t.attribute.arr_op = DIV;
			return t;
			/*assignment*/
		case C_EQUALS:
			if (b_getc(sc_buf) == C_EQUALS) {
				t.code = REL_OP_T;
				t.attribute.rel_op = EQ;
				return t;
			}
			b_retract(sc_buf);
			t.code = ASS_OP_T;
			return t;
		/*relationals*/
		case C_DOT: /*this is a bit spaghetti*/
			lexstart = b_getcoffset(sc_buf); /*index of the initial dot*/
			temp = b_getc(sc_buf);  
			if (temp == 'A') { /*if statement for .AND. conditional. Case sensitive*/
				temp = b_getc(sc_buf);
				if (temp == 'N') {
					temp = b_getc(sc_buf);
					if (temp == 'D') {
						temp = b_getc(sc_buf);
						if (temp == C_DOT) {
							t.attribute.log_op = AND;
							t.code = LOG_OP_T;
							return t;
						}
					}
				}
			}
			else if (temp == 'O') { /*if statement for .OR. conditional. Case sensitive*/
				temp = b_getc(sc_buf);
				if (temp == 'R') {
					temp = b_getc(sc_buf);
					if (temp == C_DOT) {
						t.attribute.log_op = OR;
						t.code = LOG_OP_T;
						return t;
					}
				}
			}
			temp = b_getcoffset(sc_buf) - lexstart; /*total amount of characters I've consumed past the dot*/
			while (temp) { /*retract that many times*/
				b_retract(sc_buf);
				temp--;
			}
			t.code = ERR_T; /*an incorrectly used dot: state 0 dots defined as ES in TT*/
			t.attribute.err_lex[0] = c;
			return t;
			/*end of file*/
		case C_SEOF2:
		case C_SEOF:
			t.code = SEOF_T;
			t.attribute.seof = SEOF1;
			return t;
			/*more relationals*/
		case C_GT:
			t.code = REL_OP_T;
			t.attribute.rel_op = GT;
			return t;
		case C_LT:
			t.code = REL_OP_T;
			temp = b_getc(sc_buf);
			/*Logic for <> operator*/
			if (temp == C_GT) {
				t.attribute.rel_op = NE;
				return t;
			}
			b_retract(sc_buf);
			t.attribute.rel_op = LT;
			return t;
			/*other*/
		case C_POUND:
			t.code = SCC_OP_T;
			return t;
		case C_SEMIC:
			t.code = EOS_T;
			return t;
		}
		/*Not a special case leading character: now matching a lexeme*/
		lexstart = b_mark(sc_buf, b_getcoffset(sc_buf) - 1);
		state = get_next_state(state, c, &accept);
		while (accept == NOAS) {
			c = b_getc(sc_buf);
			//printf("Got character: %c State = %i \n", c, state)
			state = get_next_state(state, c, &accept);
			//printf("New state after char: %i \n", state);
		}
		if (accept == ASWR) { b_retract(sc_buf); } /*reached accepting state*/
		lexend = b_getcoffset(sc_buf); /*end is index of final lexeme character*/
		lex_buf = b_allocate((lexend - lexstart) + 1, 0, 'f');
		if (lex_buf == NULL) {
			t.code = RTE_T;
			strcpy(t.attribute.err_lex, ERR_STRING);
			return t;
		}
		b_reset(sc_buf); /*reset getcoffset to start of lexeme so I can copy it into temporary lexeme buffer*/
		while (b_getcoffset(sc_buf) != lexend) {
			temp = b_getc(sc_buf);
			b_addc(lex_buf, temp);
		}
		b_addc(lex_buf, '\0'); /*append a \0 to the end of the lexeme to generate a c-style string*/
		//printf("My lexeme is: |%s|\n", b_location(lex_buf, 0));
		t = aa_table[state](b_location(lex_buf, 0)); /*access accepting action table at state providing argument of LEXEME*/
		b_free(lex_buf);
		return t;
	}//end while(1)
}

/*Provided in assignment specifications by Prof.*/
int get_next_state(int state, char c, int *accept)
{
	int col;
	int next;
	col = char_class(c);
	next = st_table[state][col];
#ifdef DEBUG
	printf("Input symbol: %c Row: %d Column: %d Next: %d \n", c, state, col, next);
#endif
	assert(next != IS);

#ifdef DEBUG
	if (next == IS) {
		printf("Scanner Error: Illegal state:\n");
		printf("Input symbol: %c Row: %d Column: %d\n", c, state, col);
		exit(1);
	}
#endif
	*accept = as_table[next];
	return next;
	}

/********************************************************
Purpose: Returns column in table occupied by the 'class' of the character. Character 'classes' are detailed in table.h
Author: Marco Gregory
History/Versions: 1.0
Called Functions: b_free()
Parameters: char c: a character that is a part of the platypus grammar
Return Value: int: class of character. 0=a-zA-z, 1=0, 2=1-9, 3=., 4=$, 6=", 7=SEOF, 5=else
*********************************************************/
int char_class(char c)
{
	int val;
	if ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')) { /*a-z*/
		val = 0;
	}
	else if (c == '0') { /*0*/
		val = 1;
	}
	else if ((c >= '1' && c <= '9')) { /*1-9*/
		val = 2;
	}
	else { //not alphanumeric and not special case
		switch (c) {
		case C_DOT: /*'.'*/
			val = 3;
			break;
		case C_CASH: /*'$'*/
			val = 4;
			break;
		case C_QUOTE: /*'"'*/
			val = 6;
			break;
		case C_SEOF: /*'\0'*/
			val = 7;
			break;
		default: /*other */
			val = 5;
		}
	}
	return val;
}
/********************************************************
Purpose: Arithmetic variable identifier accepting action function
Author: Marco Gregory
History/Versions: 1.0
Called Functions: getKWTPosition
Parameters: char * lexeme: a valid AVID lexeme
Return Value: Token t: a valid platypus token, code = KW_T|AVID, attribute = index in KW table|lexeme 
*********************************************************/

Token aa_func2(char * lexeme) {
	Token t; /*return token*/
	int i;
	size_t q = strlen(lexeme); /*generic container for loop and function*/
	i = getKWTPosition(lexeme);
	/*check for keyword*/
	if (i != -1) {
		t.code = KW_T;
		t.attribute.kwt_idx = i;
		return t;
	}
	/*copy set amount of characters and \0*/
	if (q > VID_LEN) {
		for (i = 0; i < VID_LEN; i++) {
			t.attribute.vid_lex[i] = lexeme[i];
		}
		t.attribute.vid_lex[VID_LEN] = C_SEOF;
	}
	else {
		for (i = 0; (size_t)i < q; i++) { /*Cast is safe, range is never out of size_t*/
			t.attribute.vid_lex[i] = lexeme[i];
		}
		t.attribute.vid_lex[i] = C_SEOF;
	}
	t.code = AVID_T;
	return t;
}
/********************************************************
Purpose: String variable identifier accepting action function
Author: Marco Gregory
History/Versions: 1.0
Called Functions: 
Parameters: char * lexeme: a valid svid lexeme
Return Value: Token t: a valid platypus token, code = SVID_T, attribute = lexeme
*********************************************************/
Token aa_func3(char *lexeme) {
	int i; /*container for loop*/
	Token t; /*return value*/
	size_t q = strlen(lexeme);
	//printf("Hi, I have a lexeme: |%s|\n", lexeme);
	/*copy set amount of characters and \0*/
	if (q > VID_LEN) {
		for (i = 0; i < VID_LEN - 1; i++) {
			t.attribute.vid_lex[i] = lexeme[i];
		}
		t.attribute.vid_lex[VID_LEN - 1] = C_CASH;
		t.attribute.vid_lex[VID_LEN] = C_SEOF;
	}
	else {
		for (i = 0; (size_t)i < q; i++) { /*Cast is safe, range is never out of size_t*/
			t.attribute.vid_lex[i] = lexeme[i];
		}
		t.attribute.vid_lex[i] = C_SEOF;
	}
	t.code = SVID_T;
	return t;
}

/********************************************************
Purpose: Floating point literal accepting action function
Author: Marco Gregory
History/Versions: 1.0
Called Functions: atof
Parameters: char * lexeme: a potentially invalid valid platypus FPL
Return Value: Token t: a valid platypus token, code = ERR_T|FPL_T, attribute = lexeme
*********************************************************/
Token aa_func6(char * lexeme) {
	double temp;/*conversion container*/
	Token t; /*return token*/
	int i; /*container for loop*/
	size_t q = strlen(lexeme);
	temp = atof(lexeme);
	if (temp == 0) { /*must compare against zero*/
		t.code = FPL_T;
		t.attribute.flt_value = (float)temp; /*Zero, safe cast*/
		return t;
	}

	if (temp > FLT_MAX || temp < FLT_MIN) {
		t.code = ERR_T;
		/*make sure token is not too long*/
		if (q > ERR_LEN) {
			for (i = 0; i < ERR_LEN - 3; i++) {
				t.attribute.err_lex[i] = lexeme[i];
			}
			for (i; i < ERR_LEN; i++) {
				t.attribute.err_lex[i] = '.';
			}
			t.attribute.err_lex[i] = C_SEOF;
			return t;
		}
		/*erroneous token is acceptable length*/
		for (i = 0; (size_t)i < q; i++) {
			t.attribute.err_lex[i] = lexeme[i];
		}
		t.attribute.err_lex[i] = C_SEOF;
		return t;
	}
	t.code = FPL_T;
	t.attribute.flt_value = (float)temp; /*safe cast*/
	return t;
}

/********************************************************
Purpose: ACCEPTING FUNCTION FOR THE integer literal(IL) - decimal constant (DIL)
Author: Marco Gregory
History/Versions: 1.0
Called Functions: atol
Parameters: char * lexeme: a potentially invalid platypus DIL
Return Value: Token t: a valid platypus token, code = INL_T|ERR_T, attribute = lexeme
*********************************************************/
Token aa_func5(char * lexeme) {
	long temp; /*conversion container*/
	Token t; /*return token*/
	int i; /*container for loop*/
	temp = atol(lexeme);
	size_t q = strlen(lexeme);

	if (temp == 0) { /*must compare against zero*/
		t.code = INL_T;
		t.attribute.int_value = temp;
		return t;
	}
	if (temp < 0 || temp > 32767) {
	//if (temp > SHRT_MAX || temp < SHRT_MIN) {
		t.code = ERR_T;
		/*make sure token is not too long*/
		if (strlen(lexeme) > ERR_LEN) {
			for (i = 0; i < ERR_LEN - 3; i++) {
				t.attribute.err_lex[i] = lexeme[i];
			}
			for (i; i < ERR_LEN; i++) {
				t.attribute.err_lex[i] = '.';
			}
			t.attribute.err_lex[i] = C_SEOF;
			return t;
		}
		/*erroneous token is acceptable length*/
		for (i = 0; (size_t)i < strlen(lexeme); i++) {
			t.attribute.err_lex[i] = lexeme[i];
		}
		t.attribute.err_lex[i] = C_SEOF;
		return t;
	}
	t.code = INL_T;
	t.attribute.int_value = (short)temp; /*cast is deemed safe because of error checkin above*/
	return t;
}
/********************************************************
Purpose: Accepting function for the platypus String Literal
Author: Marco Gregory
History/Versions: 1.0
Called Functions: b_addc
Parameters: char * lexeme: a valid platypus SL
Return Value: Token t: a valid platypus token, code = STR_T, attribute = lexeme, with quotes trimmed
*********************************************************/
Token aa_func7(char * lexeme) {
	int i = 1;/*iterator:ignoring leading '"'*/
	Token t; /*return value*/
	/*set token attributes: where the most recent token has been drawn from*/
	t.code = STR_T;
	/*add lexeme to string literal table*/
	t.attribute.str_offset = b_limit(str_LTBL);
	if (strlen(lexeme) - 1 > INT_MAX) { t.code = ERR_T; strcpy(t.attribute.err_lex, "SL out of range"); }
	/*start at index 1 and end at index -1 to eliminate quotes*/
	for (i = 1; (size_t)i < strlen(lexeme) - 1; i++) { /*Cast is safe. SL length will not be > int max so i can not overflow*/
		if (lexeme[i] == C_NL || lexeme[i] == C_CR) {
			line += 1;
		}
		if (b_addc(str_LTBL, lexeme[i]) == NULL) {
			t.code = RTE_T;
			strcpy(t.attribute.err_lex, ERR_STRING);
			return t;
		}
	}
	if (b_addc(str_LTBL, '\0') == NULL) { /*stamp a \0 on the end*/
		t.code = RTE_T;
		strcpy(t.attribute.err_lex, ERR_STRING);
		return t;
	} 
	return t;
}
/********************************************************
Purpose: Accepting function for the platypus error token
Author: Marco Gregory
History/Versions: 1.0
Called Functions: 
Parameters: char * lexeme: an erroneous platypus lexeme
Return Value: Token t: a valid platypus token, code = ERR_T, attribute = lexeme
*********************************************************/
Token aa_func8(char* lexeme) {
	Token t; /*return value*/
	int i; /*container for loop*/
	t.code = ERR_T;
	size_t q = strlen(lexeme);

	if (q > ERR_LEN) {	/*Lexeme too big*/
		for (i = 0; (size_t)i < q; i++) { /*check for whitespace*/
			if (lexeme[i] == C_NL || lexeme[i] == C_CR) { line += 1; }
		}
		for (i = 0; i < ERR_LEN - 3; i++) {
			t.attribute.err_lex[i] = lexeme[i];
		} /*append dots*/
		for (i; i < ERR_LEN; i++) {
			t.attribute.err_lex[i] = '.';
		}
	}
	else { /*lexeme of appropriate size*/
		for (i = 0; (size_t)i < q; i++) { /*cast is safe*/
			if (lexeme[i] == C_NL || lexeme[i] == C_CR) { line += 1; }
			t.attribute.err_lex[i] = lexeme[i];
		}
	}
	t.attribute.err_lex[i] = '\0';
	return t;
}
/********************************************************
Purpose: Lookup function for keyword table
Author: Marco Gregory
History/Versions: 1.0
Called Functions: b_addc
Parameters: char * lexeme: a valid platypus SL
Return Value: int i: location in the keyword table where suspected char* kw_lexeme is stored. -1: kw_lexeme is not in KW table
*********************************************************/
int getKWTPosition(char * kw_lexeme) {
	int i = 0;
	for (i = 0; i < KWT_SIZE; i++) {
		if (strcmp(kw_lexeme, kw_table[i]) == 0) {
			return i;
		}
	}
	return -1;
}