/********************************************************************************
Filename: buffer.h
List of Source/Headers: buffer.c, buffer.h, platy_bt.c

Compiler: MS VISUAL STUDIO 2017
Author: Marco Gregory	Student Number: 040876221
Course: Compilers 18F_CST8152_010
Assignment: ONE
Date: 1 October 2018
Professor: Svillen Ranev
Purpose: Contains error constants, program constants, useful macros, function declarations, bitmasks, and buffer struct for buffer.c.   
Function List: 
********************************************************************************/

#ifndef BUFFER_H_
#define BUFFER_H_
/*#define DEBUG 1 /*debug mode*/ 
/*#pragma warning(1:4001) *//*to enforce C89 type comments  - to make //comments an warning */

/*#pragma warning(error:4001)*//* to enforce C89 comments - to make // comments an error */
/* standard header files */
#include <stdio.h>  /* standard input/output */
#include <malloc.h> /* for dynamic memory allocation*/
#include <limits.h> /* implementation-defined data type ranges and limits */

/* constant definitions */
#define RT_FAIL_1 -1         /* fail return value */
#define RT_FAIL_2 -2         /* fail return value */
#define RT_FAIL_4 0x100		/*fail return value used by b_incfactor*/
#define LOAD_FAIL -2       /* load fail return value */

/* You should add your own constant definitions here */
#define	AI_MAX  255
#define MI_MAX  100

/*NuMbERs*/
#define N_ONE	-1	/*negative one*/
#define ZERO	0	/*zero*/
#define ONE		1	/*one*/
#define TWO		2	/*two*/

/*Common macros*/
#define inRange(min, max, num) num >= min && num <= max /*if we're going to repeatedly use ranges might as well standardize it*/
#define B_FULL(pBD) pBD->flags & CHECK_EOB				/*I replaced the b_eob(pBD) call in b_print with this and my output was the same*/

/* Enter your bit-masks constant definitions here */
#define DEFAULT_FLAGS	0xFFFC		/*default flags value*/
#define SET_EOB			0x1			/*set eob mask*/
#define RESET_EOB       0xFFFE		/*reset eob mask*/
#define CHECK_EOB       0x1			/*check eob mask*/
#define SET_R_FLAG		0x2			/*set r_flag mask*/
#define RESET_R_FLAG    0xFFFD		/*reset r_flag mask*/
#define CHECK_R_FLAG    0x2			/*check r_flag mask*/


/* user data type declarations */
typedef struct BufferDescriptor {
	char *cb_head;   /* pointer to the beginning of character array (character buffer) */
	short capacity;    /* current dynamic memory size (in bytes) allocated to character buffer */
	short addc_offset;  /* the offset (in chars) to the add-character location */
	short getc_offset;  /* the offset (in chars) to the get-character location */
	short markc_offset; /* the offset (in chars) to the mark location */
	char  inc_factor; /* character array increment factor */
	char  mode;       /* operational mode indicator*/
	unsigned short flags; /* contains character array reallocation flag and end-of-buffer flag */
} Buffer, *pBuffer;

/* function declarations */

Buffer * b_allocate(short init_capacity, char inc_factor, char o_mode);
pBuffer b_addc(pBuffer const pBD, char symbol);
int b_clear(Buffer* const pBD);
void b_free(Buffer* const pBD);
int b_isfull(Buffer * const pBD);
short b_limit(Buffer * const pBD);
short b_capacity(Buffer * const pBD);
short b_mark(Buffer * const pBD, short mark);
int b_mode(Buffer * const pBD);
size_t b_incfactor(Buffer * const pBD);
int b_load(FILE * const fi, Buffer * const pBD);
int b_isempty(Buffer * const pBD);
char b_getc(Buffer * const pBD);
int b_eob(Buffer * const pBD);
int b_print(Buffer* const pBD);
Buffer *b_compact(Buffer * const pBD, char symbol);
char b_rflag(Buffer* const pBD);
short b_retract(Buffer* const pBD);
short b_reset(Buffer* const pBD);
short b_getcoffset(Buffer* const pBD);
int b_rewind(Buffer* const pBD);
char* b_location(Buffer* const pBD, short loc_offset);

#endif

