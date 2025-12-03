#include "parser.h"
#include "buffer.h"
#include "token.h"
#include <stdlib.h>

//#define DEBUG 1 //I see you too are desperate. I was at one point too
#define NO_ATTR 20 //no attribute token code

extern int synerrno; //from parser.h: number of errors
extern int line;
extern Buffer * str_LTBL; /*String literal table */
extern pBuffer sc_buf; /*pointer to input source buffer*/
extern char * kw_table[];

/*Name: syn_printe
Parameters:
Return Values:
Purpose: generous gift from the professor: prints information about the lookahead token
*/
void syn_printe() {
	Token t = lookahead;
	printf("PLATY: Syntax error:  Line:%3d\n", line);
	printf("*****  Token code:%3d Attribute: ", t.code);
	switch (t.code) {
	case  ERR_T: /* ERR_T     0   Error token */
		printf("%s\n", t.attribute.err_lex);
		break;
	case  SEOF_T: /*SEOF_T    1   Source end-of-file token */
		printf("SEOF_T\t\t%d\t\n", t.attribute.seof);
		break;
	case  AVID_T: /* AVID_T    2   Arithmetic Variable identifier token */
	case  SVID_T:/* SVID_T    3  String Variable identifier token */
		printf("%s\n", t.attribute.vid_lex);
		break;
	case  FPL_T: /* FPL_T     4  Floating point literal token */
		printf("%5.1f\n", t.attribute.flt_value);
		break;
	case INL_T: /* INL_T      5   Integer literal token */
		printf("%d\n", t.attribute.get_int);
		break;
	case STR_T:/* STR_T     6   String literal token */
		printf("%s\n", b_location(str_LTBL, t.attribute.str_offset));
		break;
	case SCC_OP_T: /* 7   String concatenation operator token */
		printf("NA\n");
		break;
	case  ASS_OP_T:/* ASS_OP_T  8   Assignment operator token */
		printf("NA\n");
		break;
	case  ART_OP_T:/* ART_OP_T  9   Arithmetic operator token */
		printf("%d\n", t.attribute.get_int);
		break;
	case  REL_OP_T: /*REL_OP_T  10   Relational operator token */
		printf("%d\n", t.attribute.get_int);
		break;
	case  LOG_OP_T:/*LOG_OP_T 11  Logical operator token */
		printf("%d\n", t.attribute.get_int);
		break;
	case  LPR_T: /*LPR_T    12  Left parenthesis token */
		printf("NA\n");
		break;
	case  RPR_T: /*RPR_T    13  Right parenthesis token */
		printf("NA\n");
		break;
	case LBR_T: /*    14   Left brace token */
		printf("NA\n");
		break;
	case RBR_T: /*    15  Right brace token */
		printf("NA\n");
		break;
	case KW_T: /*     16   Keyword token */
		printf("%s\n", kw_table[t.attribute.get_int]);
		break;
	case COM_T: /* 17   Comma token */
		printf("NA\n");
		break;
	case EOS_T: /*    18  End of statement *(semi - colon) */
		printf("NA\n");
		break;
	default:
		printf("PLATY: Scanner error: invalid token code: %d\n", t.code);
	}/*end switch*/
}/* end syn_printe()*/

/*
Name: syn_eh
Parameters: int synccode: the code for the sync token
Return values:
Purpose:	disposes of tokens by advancing the lookahead one token
			until SEOF_T or the synccode is reached.
			If SEOF_T was unexpected, exit(synerrno).
			Otherwise, advance one token past the synccode
*/
void syn_eh(int sync_token_code) {
#ifdef DEBUG 
	printf("syn_eh");
#endif
	syn_printe();
	synerrno++;
	while (lookahead.code != sync_token_code) {
		lookahead = malar_next_token();
		if (lookahead.code == SEOF_T && sync_token_code == SEOF_T) {
			return;
		}
		if (lookahead.code == sync_token_code && sync_token_code != SEOF_T) {
			lookahead = malar_next_token();
			return;
		}
		if (lookahead.code == SEOF_T && sync_token_code != SEOF_T) {
			exit(synerrno);
		}		
	}
}
/*
Name: match
Parameters:	tcode: the token code to match against the lookahead
			tattr: the token attribute to match against the lookahead 
Return values: 
Purpose:	attempt to match the token code and attribute provided agianst the lookahead.
			if the code and attributes (only for KW_T, LOG_OP_T, ART_OP_T, REL_OP_T) are correct,
			advance with lookahead = malar_next_token
			if there's an error token, report and return
			if they do not match, call panic mode error handler and panic until program reaches 
			a token with code == synccode
*/
void match(int pr_token_code, int pr_token_attribute) {
#ifdef DEBUG 
	printf("match\n");
#endif
	switch (pr_token_code) {
	case KW_T:
	case LOG_OP_T:
	case ART_OP_T:
	case REL_OP_T:
		if (lookahead.attribute.get_int == pr_token_attribute) {
#ifdef DEBUG 
			print_token(lookahead);
#endif
			lookahead = malar_next_token();
			if (lookahead.code == ERR_T) {
				synerrno++;
				syn_printe();
				lookahead = malar_next_token();
			}
		}
		else {
			syn_eh(pr_token_code);
			return;
		}
		break;
	case SEOF_T:
		if (lookahead.code == SEOF_T) {
			return;
		}
		else {
			syn_eh(SEOF_T);
		}
		break;
	default:
		if (lookahead.code == pr_token_code) {
#ifdef DEBUG 
			print_token(lookahead);
#endif
			lookahead = malar_next_token();
			if (lookahead.code == ERR_T) {
				synerrno++;
				syn_printe();
				lookahead = malar_next_token();
			}
		}
		else {
			syn_eh(pr_token_code);
			return;
		}
	}
}
/*
Name: gen_incode
Parameters: char* tmp: the statement to print
Return values:
Purpose:	prints tmp and a newline.
*/
void gen_incode(char* tmp) {
	printf("%s\n", tmp);
}

/*beginning of productions list*/
/*
Name: primaryARelExpP
Parameters:
Return values:
Purpose: function representing primary arithmetic relational expression' production 
Production information:
<primary  a_relational expression’> ->
== <primary a_relational expression> |
<> <primary a_relational  expression>|
<  <primary a_relational  expression> |
>  <primary a_relational  expression>


First(<primary a_relational expression'>) = {==, <>, <, >}

*/
void primaryARelExpP() {
#ifdef DEBUG 
	printf("primaryarelexpP\n");
#endif
	//printf("primaryarelexpP\n");
	switch (lookahead.code) {
	case REL_OP_T:
		switch (lookahead.attribute.rel_op) {
		case EQ:
			match(REL_OP_T, EQ);
			primaryARelExp();
			break;
		case LT:
			match(REL_OP_T, LT);
			primaryARelExp();
			break;
		case GT:
			match(REL_OP_T, GT);
			primaryARelExp();
			break;
		case NE:
			match(REL_OP_T, NE);
			primaryARelExp();
			break;
		default:
			syn_printe();
		}
		break;
	default:
		syn_printe();
	}
}
/*
Name: primarySRelExpP
Parameters:
Return values:
Purpose: function representing primary string relational expression' production
Production information:
<primary s_relational expression'> ->
== <primary  s_relational expression> | 
<>  <primary s_relational  expression>|
<  <primary s_relational  expression>|
>  <primary s_relational  expression>
First(<primary a_relational expression'>) = {==, <>, <, >}

*/
void primarySRelExpP() {
#ifdef DEBUG 
	printf("primarySRelExpP\n");
#endif
	switch (lookahead.code) {
	case REL_OP_T:
		switch (lookahead.attribute.rel_op) {
		case EQ:
			match(REL_OP_T, EQ);
			primarySRelExp();
			//gen_incode("PLATY: Primary s_relational expression parsed\n");
			break;
		case LT:
			match(REL_OP_T, LT);
			primarySRelExp();
			//gen_incode("PLATY: Primary s_relational expression parsed\n"); 
			break;
		case GT:
			match(REL_OP_T, GT);
			primarySRelExp();
			//gen_incode("PLATY: Primary s_relational expression parsed\n"); 
			break;
		case NE:
			match(REL_OP_T, NE);
			primarySRelExp();
			//gen_incode("PLATY: Primary s_relational expression parsed\n"); 
			break;
		}
		break;
	default:
		syn_printe();
		//gen_incode("PLATY: Primary s_relational expression parsed\n");
	}
}
/*
Name: primarySRelExp
Parameters:
Return values:
Purpose: function representing primary string relational expression production
Production information:
<primary s_relational expression> ->
<primary string expression>
First(<primary s_relational expression>) = {SVID_T, STR_T}
*/
void primarySRelExp() {
#ifdef DEBUG 
	printf("primarySRelExp\n");
#endif
	primaryStringExpression();
	gen_incode("PLATY: Primary s_relational expression parsed");
}
/*
Name: primaryARelExp
Parameters:
Return values:
Purpose: function representing primary arithmetic relational expression production
Production information:
<primary a_relational expression> ->
  AVID_T
| FPL_T
| INL_T
First(<primary a_relational expression>) = {==, <>, <, >}
*/
void primaryARelExp() {
#ifdef DEBUG 
	printf("primaryARelExp\n");
#endif
	//printf("primaryarelexp\n");
	switch (lookahead.code) {
	case AVID_T:
		match(AVID_T, NO_ATTR);
		break;
	case FPL_T:
		match(FPL_T, NO_ATTR);
		break;
	case INL_T:
		match(INL_T, NO_ATTR);
		break;
	default:
		syn_printe();
		//return;
	}
	gen_incode("PLATY: Primary a_relational expression parsed");
}
/*
Name: relationalExpression
Parameters:
Return values:
Purpose: function representing relational expression production
Production information:
<relational expression> ->
<primary  a_relational expression> <primary  a_relational expression’>  |
<primary s_relational expression> <primary  s_relational expression’>
First(<relationalexpression>) ={AVID_T, FPL_T, INL_T, 
SVID_T, STR_T}

*/
void relationalExpression() {
#ifdef DEBUG 
	printf("relationalExpression\n");
#endif
	switch (lookahead.code) {
	case AVID_T:
	case FPL_T:
	case INL_T:
		primaryARelExp();
		primaryARelExpP();
		gen_incode("PLATY: Relational expression parsed");
		break;
	case STR_T:
	case SVID_T:
		primarySRelExp();
		primarySRelExpP();
		gen_incode("PLATY: Relational expression parsed");
		break;
	default:
		syn_printe();
		gen_incode("PLATY: Relational expression parsed");
	}
}
/*
Name: logORexpressionP
Parameters:
Return values:
Purpose: function representing logical or expression' production
Production information:
<logical OR expression’>
	.OR. <logical AND expression> <logical OR expression’> | epsilon
First(<logical AND expression’>) =  { .OR. , epsilon}
*/
void logORexpressionP() {
#ifdef DEBUG 
	printf("logORExpressionP\n");
#endif
	switch (lookahead.code) {
	case LOG_OP_T:
		switch (lookahead.attribute.log_op) {
		case OR:
			match(LOG_OP_T, OR);
			logANDexpression();
			logORexpressionP();
			gen_incode("PLATY: Logical OR expression parsed");
			break;
			//default: TODO: VERIFY THIS IS NOT SUPPOSED TO BE HERE
				//syn_printe();
				//break;
		}
		break;
	//default:
		//gen_incode("PLATY: Logical OR expression parsed");
	}
}
/*
Name: logANDexpressionP
Parameters:
Return values:
Purpose: function representing logical and expression' production
Production information:
<logical AND expression’> ->
	.AND. <relational expression> <logical AND expression’> | epsilon
First(<logical AND expression’>) = { .AND. , epsilon}
*/
void logANDexpressionP() {
#ifdef DEBUG 
	printf("logANDexpressionP\n");
#endif
	switch (lookahead.code) {
	case LOG_OP_T:
		switch (lookahead.attribute.log_op) {
		case AND:
			match(LOG_OP_T, AND);
			relationalExpression();
			logANDexpressionP();
			gen_incode("PLATY: Logical AND expression parsed");
			break;
		}
		break;/*
	case AVID_T:
	case FPL_T:
	case INL_T:
	case SVID_T:
	case STR_T:
		relationalExpression();
		break;*/
	}
}
/*
Name: logANDexpression
Parameters:
Return values:
Purpose: function representing logical and expression production
Production information:
<logical AND expression> ->
<relational expression> <logical AND expression’>
First(<logical AND expression’>) = { AVID_T, FPL_T, INL_T, 
SVID_T, STR_T}
*/
void logANDexpression() {
#ifdef DEBUG 
	printf("logAndexpression\n");
#endif
	relationalExpression();

	logANDexpressionP();
	//gen_incode("logANDexpression() parsed");
}
/*
Name: logORexpression
Parameters:
Return values:
Purpose: function representing logical OR expression production
Production information:
<logical OR expression> ->
	<logical AND expression> <logical OR expression’>
First(<logical OR expression>) = { AVID_T, FPL_T, INL_T,
SVID_T, STR_T}

*/
void logORexpression() {
#ifdef DEBUG 
	printf("logORexpression\n");
#endif
	logANDexpression(); 
	logORexpressionP();
	//gen_incode("PLATY: Logical OR expression parsed");
}
/*
Name: conditionalExpression
Parameters:
Return values:
Purpose: function representing conditional expression production
Production information:
<conditional expression> ->
 <logical OR  expression>
First(<conditional expression>) = { .OR. }
*/
void conditionalExpression() {
#ifdef DEBUG 
	printf("conditionalExpression\n");
#endif
	logORexpression();
	gen_incode("PLATY: Conditional expression parsed");
}
/*
Name: stringExpressionP
Parameters:
Return values:
Purpose: function representing stringExpressionP production
Production information:
<string expression’> - > # <primary string expression> <string expression’> | epsilon
First(<conditional expression>) = {#, epsilon}
*/
void stringExpressionP() {
#ifdef DEBUG 
	printf("stringExpressionP\n");
#endif
	switch (lookahead.code) {
	case SCC_OP_T:
		match(SCC_OP_T, NO_ATTR);
		primaryStringExpression();
		stringExpressionP();
		break;
	default:
		gen_incode("PLATY: String expression parsed");
	}
}
/*
Name: primaryStringExpression
Parameters:
Return values:
Purpose: function representing primary string expression production
Production information:
<primary string expression> ->
  SVID_T
| STR_T
First(<conditional expression>) = {SVID_T, STR_T}
*/
void primaryStringExpression() {
#ifdef DEBUG 
	printf("primaryStringExpression\n");
#endif
	switch (lookahead.code) {
	case SVID_T:
		match(SVID_T, NO_ATTR);
		//printf("wack1:%u\n", line);
		gen_incode("PLATY: Primary string expression parsed");
		break;
	case STR_T:
		match(STR_T, NO_ATTR);
		//printf("wack2:%u\n", line);
		gen_incode("PLATY: Primary string expression parsed");
		break;/*
	case FPL_T:
	case INL_T:
	case AVID_T:
		syn_printe();
		gen_incode("PLATY: Primary s_relational expression parsed\n");
		break;/**/
	default: 
		syn_printe();
		printf("wack3:%u\n", line);
		gen_incode("PLATY: Primary string expression parsed");
	}
}
/*
Name: stringExpression
Parameters:
Return values:
Purpose: function representing string expression production
Production information:
<string expression> ->
 <primary string expression>
| <string expression>  #  <primary string expression>
First(<conditional expression>) =  {SVID_T, STR_T}
*/
void stringExpression() {
#ifdef DEBUG 
	printf("StringExpression\n");
#endif
	primaryStringExpression();
	stringExpressionP();
}
/*
Name: primaryArithmeticExpression
Parameters:
Return values:
Purpose: function representing primary arithmetic expression production
Production information:
<primary arithmetic expression> ->
  AVID_T
| FPL_T
| INL_T
| (<arithmetic expression>)
First(<primary arithmetic expression>) = {AVID_T, FPL_T, INL_T, ( }
*/
void primaryArithmeticExpression() {
#ifdef DEBUG 
	printf("primaryArithmeticExpression\n");
#endif
	switch (lookahead.code) {
	case AVID_T:
		match(AVID_T, NO_ATTR);
		gen_incode("PLATY: Primary arithmetic expression parsed");
		break;
	case FPL_T:
		match(FPL_T, NO_ATTR);
		gen_incode("PLATY: Primary arithmetic expression parsed");
		break;
	case INL_T:
		match(INL_T, NO_ATTR);
		gen_incode("PLATY: Primary arithmetic expression parsed");
		break;
	case LPR_T:
		match(LPR_T, NO_ATTR);
		arithmeticExpression();
		match(RPR_T, NO_ATTR);
		gen_incode("PLATY: Primary arithmetic expression parsed");
		break;
	/*default: //primary arithmetic should not be able to be epsilon but I guess
		syn_printe();*/
	}
	//gen_incode("PLATY: Primary arithmetic expression parsed");
}
/*
Name: multiplicativeArithmeticExpressionP
Parameters:
Return values:
Purpose: function representing multiplicative arithmetic expression' production
Production information:
<multiplicative arithmetic expression’> ->
* <primary arithmetic expression> <multiplicative arithmetic expression’> |
/  <primary arithmetic expression> <multiplicative arithmetic expression’>|
epsilon
First(<multiplicative arithmetic expression’>) = {*, /, epsilon}
*/
void multiplicativeArithmeticExpressionP() {
#ifdef DEBUG 
	printf("multiplicativeArithmeticExpressionP\n");
#endif
	switch (lookahead.code) {
	case ART_OP_T:
		switch (lookahead.attribute.arr_op) {
		case MULT:
			match(ART_OP_T, MULT);
			primaryArithmeticExpression(); 
			multiplicativeArithmeticExpressionP();
			gen_incode("PLATY: Multiplicative arithmetic expression parsed");
			break;
		case DIV:
			match(ART_OP_T, DIV);
			primaryArithmeticExpression(); 
			multiplicativeArithmeticExpressionP();
			gen_incode("PLATY: Multiplicative arithmetic expression parsed");
			break;
		}
	}
}
/*
Name: multiplicativeArithmeticExpression
Parameters:
Return values:
Purpose: function representing multiplicative arithmetic expression production
Production information:
<multiplicative arithmetic expression> ->
<primary arithmetic expression> <multiplicative arithmetic expression’>
First(<multiplicative arithmetic expression>) =  {*, /, epsilon}
*/

void multiplicativeArithmeticExpression() {
#ifdef DEBUG 
	printf("multiplicativeArithmeticExpression\n");
#endif
	primaryArithmeticExpression(); 
	multiplicativeArithmeticExpressionP();
}
/*
Name: additiveArithmeticExpressionP
Parameters:
Return values:
Purpose: function representing multiplicative arithmetic expression production
Production information:
<additive arithmetic expression’> ->
+ <multiplicative arithmetic expression> <additive arithmetic expression’> |
-  <multiplicative arithmetic expression> <additive arithmetic expression’> |
epsilon
First(<additive arithmetic expression’>) =  {+, -, epsilon}
*/
void additiveArithmeticExpressionP() {
#ifdef DEBUG 
	printf("additiveArithmeticExpressionP\n");
#endif
	switch (lookahead.code) {
	case ART_OP_T:
		switch (lookahead.attribute.arr_op) {
		case PLUS:
			/*received an error token after this arithmetic operator: . in ass3w2 expression*/
			match(ART_OP_T, PLUS);
			multiplicativeArithmeticExpression(); 
			additiveArithmeticExpressionP();
			gen_incode("PLATY: Additive arithmetic expression parsed");
			break;
		case MINUS:	
			match(ART_OP_T, MINUS);
			multiplicativeArithmeticExpression(); 
			additiveArithmeticExpressionP();
			gen_incode("PLATY: Additive arithmetic expression parsed");
			break;
		}
	//default:
	//	gen_incode("additiveArithmeticExpressionP() parsed");
	}
}
/*
Name: additiveArithmeticExpression
Parameters:
Return values:
Purpose: function representing multiplicative arithmetic expression production
Production information:
<additive arithmetic expression> ->
<multiplicative arithmetic expression> <additive arithmetic expression’> 

First(<additive arithmetic expression>) = {AVID_T, FPL_T, INL_T, ( }
*/
void additiveArithmeticExpression() {
#ifdef DEBUG 
	printf("additiveArithmeticExpression\n");
#endif
	multiplicativeArithmeticExpression(); 
	additiveArithmeticExpressionP();
	//gen_incode("PLATY: Additive arithmetic expression parsed");
}
/*
Name: unaryArithmeticExpression
Parameters:
Return values:
Purpose: function representing unary arithmetic expression production
Production information:
<unary arithmetic expression> ->
   -  <primary arithmetic expression>
| + <primary arithmetic expression>
First(<unary arithmetic expression>) = {+,-}
*/
void unaryArithmeticExpression() {
#ifdef DEBUG 
	printf("unaryArithmeticExpression\n");
#endif
	switch (lookahead.code) {
	case ART_OP_T:
		switch (lookahead.attribute.arr_op) {
		case PLUS:
			match(ART_OP_T, PLUS);
			primaryArithmeticExpression();
			break;
		case MINUS:
			match(ART_OP_T, MINUS);
			primaryArithmeticExpression();
			break;
		default:
			syn_printe();
		}
		break;
	default:
		break;
	}
	gen_incode("PLATY: Unary arithmetic expression parsed");
}
/*
Name: arithmeticExpression
Parameters:
Return values:
Purpose: function representing arithmetic expression production
Production information:
arithmetic expression> - >
  <unary arithmetic expression>
| <additive arithmetic expression>
First(<arithmetic expression>) = {+,-, AVID_T, FPL_T, INL_T, ( }
*/
void arithmeticExpression() {
#ifdef DEBUG 
	printf("arithmeticExpression\n");
#endif
	switch (lookahead.code) {
	case ART_OP_T: /*beginning of a unary: consume leading +/-*/
		switch (lookahead.attribute.arr_op)
		{
		case MINUS:
		case PLUS:
			unaryArithmeticExpression();
			break;
		default:
			syn_printe();
			break;
		}
		gen_incode("PLATY: Arithmetic expression parsed");
		break;
	case AVID_T:
	case FPL_T:
	case INL_T: /*it is additive */
		//additiveArithmeticExpression();
		//break;
	case LPR_T: ///*open bracket and recurse*/
		additiveArithmeticExpression();
		gen_incode("PLATY: Arithmetic expression parsed");
		break;
	default:
		syn_printe();
	}
	//gen_incode("PLATY: Arithmetic expression parsed");
}
/*
Name: w_args
Parameters:
Return values:
Purpose: function representing w_args production
Production information:
<w_args> -> <variable list> | STR_T | epsilon
First(<arithmetic expression>) = {AVID_T, SVID_T, STR_T, epsilon}
*/
void w_args() {
#ifdef DEBUG 
	printf("w_args\n");
#endif
	switch (lookahead.code) {
	case AVID_T:
	case SVID_T:
		variableList();
		//gen_incode("PLATY: Variable list parsed");
		break;
	case STR_T:
		match(STR_T, NO_ATTR);
		gen_incode("PLATY: Output list (string literal) parsed");
		break;
	default:
		gen_incode("PLATY: Output list (empty) parsed");
	}
}
/*
Name: outputStatement
Parameters:
Return values:
Purpose: function representing output statement production
Production information:
<output statement> -> WRITE(<w_args>);
First(<output statement>) = {WRITE}
*/
void outputStatement() {
#ifdef DEBUG 
	printf("outputtStatement\n");
#endif
	match(KW_T, WRITE);
	match(LPR_T, NO_ATTR);
	w_args();
	match(RPR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Output statement parsed");
}
/*
Name: variableIdentifier
Parameters:
Return values:
Purpose: function representing variable identifier production
Production information:
<variable list> -> <variable identifier> <variable list’>
First(<variable identifier>) =  {AVID_T, SVID_T}

*/
void variableIdentifier() {
#ifdef DEBUG 
	printf("variableIdentifier\n");
#endif
	switch (lookahead.code) {
	case AVID_T:
		match(AVID_T, NO_ATTR);
		break;
	case SVID_T:
		match(SVID_T, NO_ATTR);
		break;
	default:
		syn_printe();
	}
	//gen_incode("variableIdentifier() parsed");
}
/*
Name: variableListP
Parameters:
Return values:
Purpose: function representing variable list' production
Production information:
<variable list’> -> , <variable identifier> <variable list’> | epsilon
First(<variable list’>) = { , , epsilon }

*/
void variableListP() {
#ifdef DEBUG 
	printf("variableListP\n");
#endif
	switch (lookahead.code) {
	case COM_T:
		match(COM_T, NO_ATTR);
		variableIdentifier();
		variableListP();
		break;
	default:
		break;
	//case AVID_T:
	//case SVID_T:
		
		//break;
	//default:
		//gen_incode("variableListP() parsed");
	}
}
/*
Name: variableList
Parameters:
Return values:
Purpose: function representing variable list production
Production information:
<variable list> -> <variable identifier> <variable list’>
First(<variable list>) = {AVID_T, SVID_T}
*/
void variableList() {
#ifdef DEBUG 
	printf("varialbEList\n");
#endif
	variableIdentifier(); variableListP();
	gen_incode("PLATY: Variable list parsed");
}
/*
Name: inputStatement
Parameters:
Return values:
Purpose: function representing input statement production
Production information:
<input statement> ->
READ (<variable list>);

First(<input statement>) = {READ}
*/
void inputStatement() {
#ifdef DEBUG 
	printf("inputStatement\n");
#endif
	match(KW_T, READ);
	match(LPR_T, NO_ATTR);
	variableList();
	match(RPR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Input statement parsed");
}
/*
Name:  preCondition
Parameters:
Return values:
Purpose: function representing pre-condition production
Production information:
<pre-condition> ->
	TRUE | FALSE
First(<pre-condition>) = {TRUE, FALSE}

*/
void preCondition() {
#ifdef DEBUG 
	printf("preCondition\n");
#endif
	switch (lookahead.code) {
	case KW_T:
		if (lookahead.attribute.get_int == TRUE) {
			match(KW_T, TRUE);
			break;
		} else if (lookahead.attribute.get_int == FALSE) {
			match(KW_T, FALSE);
			break;
		}
	default:
		syn_printe();
	}
	//gen_incode("preCondition() parsed");
}
/*
Name: statementP
Parameters:
Return values:
Purpose: function representing statement' production
Production information:
<statement’> -> <statement> <statement’> | epsilon
First(<statement’>) =  {AVID, SVID, IF, WHILE, READ, WRITE, epsilon}
*/
void statementP() {
	/* FIRST set: {AVID_T,SVID_T,KW_T(but not … see above),e} */
	switch (lookahead.code) {
	case AVID_T:
	case SVID_T:
		statement();
		statementP();
		break;
	case KW_T:
		if (lookahead.attribute.get_int != PLATYPUS && lookahead.attribute.get_int != ELSE && lookahead.attribute.get_int != THEN && lookahead.attribute.get_int != REPEAT && lookahead.attribute.get_int != TRUE && lookahead.attribute.get_int != FALSE)
		{
			statement();
			statementP();
			break;
		}
	//default: /*empty string – optional statements*/;
		//gen_incode("PLATY: Opt_statements parsed");
	}
}
/*
Name: iterationStatement
Parameters:
Return values:
Purpose: function representing Iteration Statement production
Production information:
<iteration statement> ->
		  WHILE <pre-condition> (<conditional expression>)
		  REPEAT { <statements>};
First(<iteration statement>) =  {WHILE}
*/
void iterationStatement() {
	match(KW_T, WHILE);
	preCondition();
	match(LPR_T, NO_ATTR);
	conditionalExpression();
	match(RPR_T, NO_ATTR);
	match(KW_T, REPEAT);
	match(LBR_T, NO_ATTR);
	statements();
	match(RBR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Iteration statement parsed");
}
/*
Name: selectionStatement
Parameters:
Return values:
Purpose: function representing selection statement production
Production information:
<selection statement> ->
  IF <pre-condition>  (<conditional expression>) THEN { <opt_statements> }
  ELSE { <opt_statements> } ;
First(<selection statement>) =  {IF}
*/
void selectionStatement() {
	match(KW_T, IF);
	preCondition();
	match(LPR_T, NO_ATTR);
	conditionalExpression();
	match(RPR_T, NO_ATTR);
	match(KW_T, THEN);
	match(LBR_T, NO_ATTR);
	opt_statements();
	match(RBR_T, NO_ATTR);
	match(KW_T, ELSE);
	match(LBR_T, NO_ATTR);
	opt_statements();
	match(RBR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Selection statement parsed");
}
/*
Name: assignmentExpression
Parameters:
Return values:
Purpose: function representing assignment expression production
Production information:
< assignment expression> ->
  AVID = <arithmetic expression>
| SVID = <string expression>
First(<assignment expression>) =  {AVID, SVID}
*/
void assignmentExpression() {
	switch (lookahead.code) {
	case AVID_T:
		match(AVID_T, NO_ATTR);
		match(ASS_OP_T, NO_ATTR);
		arithmeticExpression();
		gen_incode("PLATY: Assignment expression (arithmetic) parsed");
		break;
	case SVID_T:
		match(SVID_T, NO_ATTR);
		match(ASS_OP_T, NO_ATTR);
		stringExpression();
		gen_incode("PLATY: Assignment expression (string) parsed");
		break;
	default:
		syn_printe();
	}
	//gen_incode("assignmentExpression() parsed");
}
/*
Name: assignmentStatement
Parameters:
Return values:
Purpose: function representing assignment statement production
Production information:
<assignment statement> ->
	<assignment expression>;
First(<assignment expression>) = {AVID, SVID}
*/
void assignmentStatement() {
#ifdef DEBUG 
	printf("assignmentStatement\n");
#endif
	assignmentExpression();
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Assignment statement parsed");
}
/*
Name: statements
Parameters:
Return values:
Purpose: function representing statements production
Production information:
<statements> -> <statement> <statement’>
First(<assignment expression>) = {AVID, SVID, IF, WHILE, READ, WRITE}
*/
void statements() {
	statement();
	statementP();
	//gen_incode("PLATY: Program Parsed");
}
/*
Name: statement
Parameters:
Return values:
Purpose: function representing statement production
Production information:
<statement> ->
  <assignment statement>
| <selection statement>
| <iteration statement>
	 | <input statement>
| <output statement>

First(<statement>) = {AVID, SVID, IF, WHILE, READ, WRITE}
*/
void statement() {
	switch (lookahead.code) {
	case AVID_T:
	case SVID_T:
		assignmentStatement();
		break;
	case KW_T:
		/* check for PLATYPUS, ELSE, THEN, REPEAT, TRUE, FALSE here      and in statements_p()*/
		if (lookahead.attribute.get_int != PLATYPUS && lookahead.attribute.get_int != ELSE && lookahead.attribute.get_int != THEN && lookahead.attribute.get_int != REPEAT && lookahead.attribute.get_int != TRUE && lookahead.attribute.get_int != FALSE)
		{
			switch (lookahead.attribute.get_int) {
			case IF:
				selectionStatement();
				break;
			case WHILE:
				iterationStatement();
				break;
			case READ:
				inputStatement();
				break;
			case WRITE:
				outputStatement();
				break;
			//default:
				/*literally impossible*/
				//printf("This is horrible. Everything is wrong. This should never happen. What did you do to cause this?");
			}
		}
		break;
	default: 
		syn_printe();
	}
	//gen_incode("PLATY: Opt_statements parsed");
}
/*
Name: opt_statements
Parameters:
Return values:
Purpose: function representing opt_statements production
Production information:
<opt_statements>
<statements> | epsilon
First(<opt_statements>) =  {AVID, SVID, IF, WHILE, READ, WRITE, epsilon}
*/
void opt_statements() {
	/* FIRST set: {AVID_T,SVID_T,KW_T(but not … see above),e} */
	switch (lookahead.code) {
	case AVID_T:
	case SVID_T:
		statements();
		break;
	case KW_T:
		/* check for PLATYPUS, ELSE, THEN, REPEAT, TRUE, FALSE here and in statements_p()*/
		if (lookahead.attribute.get_int != PLATYPUS && lookahead.attribute.get_int != ELSE && lookahead.attribute.get_int != THEN && lookahead.attribute.get_int != REPEAT && lookahead.attribute.get_int != TRUE && lookahead.attribute.get_int != FALSE)
		{
			statements();
			break;
		}
	default: /*empty string – optional statements*/;
		gen_incode("PLATY: Opt_statements parsed");
	}
}
/*
Name: program
Parameters:
Return values:
Purpose: function representing program 
Production information:
<program>  ->
	  PLATYPUS {<opt_statements>}
First(<program>) =  { PLATYPUS }
*/
void program() {
	match(KW_T, PLATYPUS);/*platypus*/
	match(LBR_T, NO_ATTR); 
	opt_statements(); 
	match(RBR_T, NO_ATTR);  
	gen_incode("PLATY: Program parsed");
}
/*
Name: program
Parameters:
Return values:
Purpose: function that begins parser: calls program(), which parses source 
*/
void parser()
{
	lookahead = malar_next_token();
	program();
	match(SEOF_T, NO_ATTR);
	gen_incode("PLATY: Source file parsed");
}
