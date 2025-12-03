**3	The PLATYPUS Syntactic Specification**  
The ***syntactic grammar*** for PLATYPUS is given below. This grammar has **PLATYPUS** tokens defined by the lexical grammar as its terminal symbols. For the sake of readability, the corresponding lexemes are used in lieu of the tokens for keywords, separators, and operators.  For example, the lexeme **\+** is used instead of **ART\_OP\_T** with an attribute **PLUS**. The ***syntactic grammar*** is to define a set of productions \- starting from the start symbol \<program\> \- that describe how sequences of tokens can form syntactically correct PLATYPUS programs

**3.1 	PLATYPUS Program**

A **PLATYPUS** program is a sequence of statements \- no statements at all, one statement, or more than one statement, enclosed in braces { }. The compilation unit is a single file containing one program and terminated by the SEOF character (SEOF\_T token).

\<opt\_statements\>   
\<statements\> | ϵ

\<program\>  \-\>  
  	PLATYPUS {\<opt\_statements\>} 

***Left Recursion in statements***  
***\<statements\> \-\> \<statement\> \<statement’\>***  
***\<statement’\> \-\> \<statement\> \<statement’\> | ϵ***

***First(\<TERM\>):***  
***\<program\> 		\= { PLATYPUS }***  
***\<statements\> 	\= {First(statement)}***  
 	***\= {First(assignment), First(selection), First(iteration), First(input),***   
***First(output)}***  
***\=  {AVID, SVID, IF, WHILE, READ, WRITE}***  
***\<opt\_statements\>	\= {First(statement), ϵ}***  
			***\= {AVID, SVID, IF, WHILE, READ, WRITE, ϵ}***  
***\<statement’\>	\= {AVID, SVID, IF, WHILE, READ, WRITE, ϵ}***

**3.2 	Statements**

The sequence of execution of a PLATYPUS program is controlled by statements. Some statements contain other statements as part of their structure; such other statements are substatements of the statement. PLATYPUS supports the following five types of statements: assignment, selection, iteration, input and output statements.

\<statement\> \-\>  
  \<assignment statement\>  
| \<selection statement\>  
| \<iteration statement\>  
 	| \<input statement\>  
| \<output statement\>

***First(\<statement\>) 			\= {First(assignment statement), First(selection***   
***statement), First(iteration), First(input),***   
***First(output)}***  
***\= {AVID, SVID, IF, WHILE, READ, WRITE}***

1. **Assignment Statement**

\<assignment statement\> \-\>   
	\<assignment expression\>;

\< assignment expression\> \-\>   
  AVID \= \<arithmetic expression\>  
| SVID \= \<string expression\>

***First(\<TERM\>):***

***\<assignment expression\>	\= {AVID, SVID}***  
***\<assignment statement\>  	\= {First(assignment expression)}***  
				***\= {AVID, SVID}***

The assignment statement is evaluated in the following order. First, the assignment expression on the right side of the assignment operator is evaluated. Second, the result from the evaluation is stored into the variable on the left side of the assignment operator. If the assignment expression is of arithmetic type and the data types of the variable and the result are different, the result is converted to the variable type implicitly.   
String expressions operate on strings only and no conversions are allowed.

**3.2.2  Selection Statement( the if statement)**  
The ***selection statement*** is an alternative selection statement, that is, there are two possible selections.  
If the *conditional expression* evaluates to true and the *pre-condition* is the keyword **TRUE**, the statements (if any) contained in the **THEN** clause are executed and the execution of the program continues with the statement following the selection statement. If the *conditional expression* evaluates to false, only the statement (if any) contained in the ELSE clause are executed and the execution of the program continues with the statement following the selection statement.  
If the *conditional expression* evaluates to false and the *pre-condition* is the keyword **FALSE**, the statements (if any) contained in the **THEN** clause are executed and the execution of the program continues with the statement following the selection statement. If the *conditional expression* evaluates to true, only the statement (if any) contained in the ELSE clause are executed and the execution of the program continues with the statement following the selection statement.  
Both **THAN** and **ELSE** clauses must be present but may be empty – no statements at all.

\<selection statement\> \-\>  
  IF \<pre-condition\>  (\<conditional expression\>) THEN { \<opt\_statements\> }  
  ELSE { \<opt\_statements\> } ;

***First(\<TERM\>):***

***\<selection statement\> 		\=  {IF}***  
***\<pre-condition\>			\= {TRUE, FALSE}***  
***\<conditional expression\>	\= {First(\<logical or expression\>)}***  
					***\= { .OR. }***  
***\<opt\_statements\>			\= {First(statements), ϵ}***  
					***\= {AVID, SVID, IF, WHILE, READ, WRITE, ϵ}***

**3.2.3  Iteration Statement (the loop statement)**

The ***iteration statement*** is used to implement iteration control structures. The ***iteration statement*** executes repeatedly the statements specified by the **REPEAT** clause of the **WHILE** loop depending on the *pre-condition* and *conditional expression*. If the *pre-condition* is the keyword **TRUE**, the statements are repeated until the evaluation of the *conditional expression* becomes false. If the *pre-condition* is the keyword **FALSE**, the statements are repeated until the evaluation of the *conditional expression* becomes true.

\<iteration statement\> \-\>  
          WHILE **\<**pre-condition\> **(\<**conditional expression\>**)**  
          REPEAT **{** \<statements*\>***};**

**\<**pre-condition\> \-\>  
	TRUE | FALSE

***First(\<TERM\>):***

***\<iteration statement\> 	\= {WHILE}***  
***\<pre-condition\>		\= {TRUE, FALSE}***

**3.2.4 Input Statement**   
The ***input statement*** reads a floating-point, an integer or a string literal from the standard input and stores it into a floating-point, an integer variable or a string variable.

\<input statement\> \-\>  
READ (\<variable list\>);

\<variable list\> \-\>  
\<variable identifier\> | \<variable list\>,\<variable identifier\>

***Left recursion in variable list***

***\<variable list\> \-\> \<variable identifier\> \<variable list’\>***  
***\<variable list’\> \-\> , \<variable identifier\> \<variable list’\> | ϵ***

***First(\<TERM\>):***

***\<input statement\>		\= {READ}***  
***\<variable list\>		\= {first(variable identifier)}***  
				***\= {AVID\_T, SVID\_T}***  
***\<variable list’\>		\= { , , ϵ }***  
***\<variable identifier\>	\= {AVID\_T, SVID\_T}***

**3.2.5 Output Statement** 

The ***output statement*** writes a variable list or a string to the standard output. Output statement with an empty variable list prints an empty line.

\<output statement\> \-\>  
  WRITE (\<*opt\_variable list\>*);  
| WRITE (STR\_T);

***Left factoring in output statement***

***\<output statement\> \-\> WRITE(\<w\_args\>);***  
***\<w\_args\> \-\> \<variable list\> | STR\_T | ϵ***

***First(\<TERM\>):***

***\<output statement\>	\= {WRITE}***  
***\<w\_args\>			\= {First(variable list), STR\_T, ϵ}***  
					***\= {AVID\_T, SVID\_T, STR\_T, ϵ}***

**3.3	Expressions**

Most of the work in a PLATYPUS program is done by evaluating expressions, either for their side effects, such as assignments to variables, or for their values, which can be used as operands in larger expressions, or to affect the execution sequence in statements, or both.

This section specifies the meanings of PLATYPUS expressions and the rules for their evaluation. 

An expression is a sequence of operators and operands that specifies a computation. When an expression in a PLATYPUS program is *evaluated* (*executed*), the result denotes a value. There are four of expressions in the PLATYPUS language: arithmetic expression, string expressions, relational expressions, and conditional expression.  
The expressions are always evaluated from left to right.

**3.3.1 Arithmetic Expression**

 An ***arithmetic expression*** is an infix expression constructed from arithmetic variables, arithmetic literals, and the operators *plus* (+), *minus* (-), *multiplication* (\*), and *division* (/). The arithmetic expression always evaluates either to a floating-point value or to an integer value. Mixed type arithmetic expressions and mixed arithmetic assignments are allowed.   
The data type of the result of the evaluation is determined by the data types of the operands. If there is at least one floating-point operand, all operands are converted to floating-point type, the operations are performed as floating-point, and the type of the result is floating-point.

The type conversion (coercion) is implicit.  All operators are left associative. Plus and minus operators have the same order of precedence. Multiplication and division have the same order of precedence but they have a higher precedence than plus and minus operators. Plus and minus can be used as unary operator to change the sign of a value. In this case they have the highest order of precedence and they are evaluated first.

The formal syntax of the arithmetic expression is listed below.

\<arithmetic expression\> \- \>  
  \<unary arithmetic expression\>    
| \<additive arithmetic expression\>	

\<unary arithmetic expression\> \-\>  
   \-  \<primary arithmetic expression\>   
| \+ \<primary arithmetic expression\>

\<additive arithmetic expression\> \-\>  
  \<additive arithmetic expression\> \+  \<multiplicative arithmetic expression\>

| \<additive arithmetic expression\>  \-  \<multiplicative arithmetic expression\>  
| \<multiplicative arithmetic expression\>  
    
\<multiplicative arithmetic expression\> \-\>  
 \<multiplicative arithmetic expression\> \* \<primary arithmetic expression\>  
| \<multiplicative arithmetic expression\> / \<primary arithmetic expression\>  
| \<primary arithmetic expression\>

\<primary arithmetic expression\> \-\>  
  AVID\_T  
| FPL\_T  
| INL\_T  
| (\<arithmetic expression\>)	

***Left recursion in additive arithmetic expression, multiplicative arithmetic expression***

***\<multiplicative arithmetic expression\> \-\>***  
***\<primary arithmetic expression\> \<multiplicative arithmetic expression’\>*** 

***\<multiplicative arithmetic expression’\> \-\>***   
***\* \<primary arithmetic expression\> \<multiplicative arithmetic expression’\> |***  
***/  \<primary arithmetic expression\> \<multiplicative arithmetic expression’\>|***  
***ϵ***  
***\<additive arithmetic expression\> \-\>***  
***\<multiplicative arithmetic expression\> \<additive arithmetic expression’\>*** 

***\<additive arithmetic expression’\> \-\>***   
***\+ \<multiplicative arithmetic expression\> \<additive arithmetic expression’\> |***  
***\-  \<multiplicative arithmetic expression\> \<additive arithmetic expression’\> |***  
***ϵ***

***First(\<TERM\>):***  
***\<arithmetic expression\>				\= first(\<unary arithmetic expression\>),***  
***first(\<additive arithmetic expression\>)***  
***\= {+,-, AVID\_T, FPL\_T, INL\_T, ( }***  
***\<unary arithmetic expression\>			\= {+,-}***  
***\<primary arithmetic expression\> 		\= {AVID\_T, FPL\_T, INL\_T, ( }***  
***\<additive arithmetic expression\>		\= {first(primary arithmetic expression)}***  
							***\= {AVID\_T, FPL\_T, INL\_T, ( }***  
***\<additive arithmetic expression’\>		\= {+, \-, ϵ}***  
***\<multiplicative arithmetic expression’\>	\= {\*, /, ϵ}***

**3.3.2 String Expression**

A ***string expression*** is an infix expression constructed from string variables, string literals, and the operator *append* ***or concatenation (\<\>).*** ??? \<\> \= NE operator? The string expression always evaluates to a string (or a pointer to string).  The append operator is left associative.

\<string expression\> \-\>  
 \<primary string expression\>  
| \<string expression\>  \#  \<primary string expression\>

\<primary string expression\> \-\>  
  SVID\_T  
| STR\_T

***Left recursion in string expression***

***\<string expression\> \-\>***  
***\<primary string exprression\> \<string expression’\>*** 

***\<string expression’\> \- \> \# \<primary string expression\> \<string expression’\> | ϵ***

***First(\<TERM\>):***  
***\<primary string expression\>			\= {SVID\_T, STR\_T}***  
***\<string expression\>				\= {first(primary string expression)}***  
							***\= {SVID\_T, STR\_T}***  
***\<string expression’\> 				\= {\#, ϵ}***

**3.3.3 Conditional Expression**

 A ***conditional expression*** is an infix expression constructed from relational expressions and the logical operators **.AND.** and/or **.OR.**. The logical operator **.AND.** has a higher order of precedence than **.OR.**. Parentheses are not allowed in the conditional expressions, thus the evaluation order cannot be changed. All operators are left associative

The conditional expressions evaluate to true or false. The internal representation of the values of true and false are left to the implementation.

The formal syntax of the conditional expression follows.

\<conditional expression\> \-\>  
 \<logical OR  expression\>

\<logical  OR expression\> \-\>  
   \<logical AND expression\>  
   | \<logical OR expression\>  .OR.  \<logical AND expression\>

\<logical AND expression\> \-\>  
   \<relational expression\>  
   | \<logical AND expression\> .AND.  \<relational expression\>	 

***Left Recursion in logical AND expression,*** 

***\<logical AND expression\> \-\>***   
***\<relational expression\> \<logical AND expression’\>***

***\<logical AND expression’\> \-\>***   
	***.AND. \<relational expression\> \<logical AND expression’\> | ϵ***

***\<logical OR expression\> \-\>***   
	***\<logical AND expression\> \<logical OR expression’\>***

***\<logical OR expression’\>***  
	***.OR. \<logical AND expression\> \<logical OR expression’\> | ϵ***

***First(\<TERM\>):***  
***\<logical OR expression’\>				\= { .OR. , ϵ}***  
***\<logical AND expression’\>			\= { .AND. , ϵ}***  
***\<logical AND expression\>			\= first(relational expression)***  
							***\= { AVID\_T, FPL\_T, INL\_T,***   
***SVID\_T, STR\_T}***  
***\<logical OR expression\> \-\> 			\= first(relational expression)***  
							***\= { AVID\_T, FPL\_T, INL\_T,***   
***SVID\_T, STR\_T}***

**3.3.4 Relational Expression**

A ***relational expression*** is an infix expression constructed from variable identifiers (VID), literals (constants), and comparison operators (==, \<\>, \<, \>). The comparison operators have a higher order of precedence than the logical operators do.

The relational expressions evaluate to true or false.

The formal syntax of the relational expression follows.

\<relational expression\> \-\>  
 \<primary a\_relational expression\>  \==  \<primary a\_relational expression\>  
| \<primary a\_relational  expression\>  \<\>  \<primary a\_relational  expression\>

| \<primary a\_relational  expression\>  \>   \<primary a\_relational  expression\>  
| \<primary a\_relational expression\>  \<   \<primary a\_relational expression\>  
| \<primary s\_relational expression\>  \==  \<primary s\_relational expression\>  
| \<primary s\_relational  expression\>  \<\>  \<primary s\_relational  expression\>

| \<primary s\_relational  expression\>  \>   \<primary s\_relational  expression\>  
| \<primary s\_relational expression\>  \<   \<primary s\_relational expression\>

***Relational Expression contains left factoring in \<\>/\< symbols and in multiple primary x\_relational***

***\<relational expression\> \-\>***   
 	***\<primary  a\_relational expression\> \<primary  a\_relational expression’\>  |***   
***\<primary s\_relational expression\> \<primary  s\_relational expression’\>*** 

***\<primary  a\_relational expression’\> \-\>***   
	***\== \<primary  a\_relational expression\> |***   
***\<\>  \<primary a\_relational  expression\>|***  
***\<  \<primary a\_relational  expression\>|***  
***\>  \<primary a\_relational  expression\>***

***\<primary  s\_relational expression’\> \-\>***   
	***\== \<primary  s\_relational expression\> |***   
***\<\>  \<primary s\_relational  expression\>|***  
***\<  \<primary s\_relational  expression\>|***  
***\>  \<primary s\_relational  expression\>***

\<primary a\_relational expression\> \-\>  
  AVID\_T  
| FPL\_T  
| INL\_T  
	  
\<primary s\_relational expression\> \-\>  
\<primary string expression\>

***FIRST(\<TERM\>):***  
***\<primary a\_relational expression\> 				\= {AVID\_T, FPL\_T, INL\_T}***  
***\<primary s\_relational expression\> 				\= First(primary string***   
									***expression)***  
									***\= {SVID\_T, STR\_T}***  
***\<primary  s\_relational expression’\> 				\= {==, \<\>, \<, \>}***  
***\<primary  a\_relational expression’\> 				\= {==, \<\>, \<, \>}***  
***\<relational expression\>						\= {AVID\_T, FPL\_T, INL\_T,***   
***SVID\_T, STR\_T}***

Enjoy the PLATYPUS grammar and do not forget that:

*“O hateful error, melancholy’s child\!*  
*Why dost thou show, to the apt thoughts of men?*  
*The things that are not?”*

W. Shakespeare, *Julius Caesar*

CST8152 – Compilers, F18, S^R

	