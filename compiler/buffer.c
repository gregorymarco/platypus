/********************************************************************************
Filename: buffer.c
List of Source/Headers: buffer.c, buffer.h, platy_bt.c

Compiler: MS VISUAL STUDIO 2017
Author: Marco Gregory	Student Number: 040876221
Course: Compilers 18F_CST8152_010
Assignment: ONE
Date: 
Professor: Svillen Ranev
Purpose: To create a dynamic character buffer capable of taking in sequences of characters and providing utilities to deal with them in various ways.
Function List: b_allocate(), b_addc(), b_clear(), b_free(), b_isfull(), b_limit(), b_capacity(), b_mark(), b_mode(), b_incfactor()
b_load(), b_isempty(), b_getc(), b_eob(), b_print(), b_compact(), b_rflag(), b_retract(), b_reset(), b_getcoffset(), b_rewind(), b_location()

Note:All functions with in parameter Buffer* pBD assume that pBD is a valid buffer struct allocated with b_allocate. Other types will cause your program to crash! 

********************************************************************************/
#define _CRT_SECURE_NO_WARNINGS

#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include "buffer.h"

#ifdef _MSC_VER
#define _CRTDBG_MAP_ALLOC
#include <crtdbg.h>
#endif

/********************************************************
Purpose: Allocator for buffer struct. 
Author: Marco Gregory
History/Versions: 1.0
Called Functions: b_free()
Parameters: init_capacity: 1 to SHRT_MAX-1 inclusive ( # of initial bytes ), inc_factor: 1-255 inclusive for ( bytes ) 
Return Value: NULL, buffer*
Algorithm: 
	-Validates that values are in the correct range
	-Allocates memory for buffer struct and initializes all values. 
	-Allocates memory for new cb_head with number_bytes = init_capacity
	-Sets mode, flags, and capacity.
	-Returns struct
	-If at any point it fails, it frees allocated memory and returns NULL
*********************************************************/
Buffer * b_allocate(short init_capacity, char inc_factor, char o_mode) {
	Buffer* temp_buf; //buffer who we are allocating dynamic memory for & initializing to zero
	//verify that init_capacity is valid size before hassling with memory

	if (!(inRange(ZERO, SHRT_MAX - ONE, init_capacity))) {
		#ifdef DEBUG
		printf("%s", "buffer: Cannot allocate buffer\n");
		#endif
		return NULL;
	}
	if (inc_factor == ZERO) { //it is actually a fixed increment buffer
		o_mode = 'f';
	}
	//allocate memory for temporary buffer struct to be returned
	temp_buf = (Buffer*)calloc(ONE, sizeof(Buffer));
	if (temp_buf == NULL) {
		printf("%s", "buffer: Cannot allocate buffer\n");
		return NULL;
	}
	//allocate memory for cb_head: init_capacity is used 
	temp_buf->cb_head = (char*)malloc(init_capacity);
	if (temp_buf->cb_head == NULL) {
		free(temp_buf);
		printf("%s", "buffer: Cannot allocate buffer\n");
		return NULL;
	}
	//set operational mode based on o_mode parameter
	switch (o_mode) {
		case 'f':
			temp_buf->mode = ZERO;
			inc_factor = ZERO;
			break;
		case 'a': 
			//check that range is valid
			if (inRange(ONE, AI_MAX, (unsigned char)inc_factor)) {
				temp_buf->mode = ONE;
				temp_buf->inc_factor = inc_factor;
			}
			else {
				//printf("buffer: inc_factor provided: %i not in valid range 0-255 inclusive\n", inc_factor);
				printf("%s", "buffer: Cannot allocate buffer\n");
				b_free(temp_buf);
				return NULL;
			}
			break;
		case 'm':
			//check that range is valid
			if (inRange(ONE, MI_MAX, (unsigned char) inc_factor)) {
				temp_buf->mode = N_ONE;
				temp_buf->inc_factor = inc_factor;
			}
			else {
				#ifdef DEBUG
				printf("buffer: inc_factor provided: %i not in valid range 1-100 inclusive\n", inc_factor);
				#endif // DEBUG
				printf("%s", "buffer: Cannot allocate buffer\n");
				b_free(temp_buf);
				return NULL;
			}
			break;
		default:
			#ifdef DEBUG
			printf("%s", "buffer: flags invalid. Valid values(char): f, a, m");
			#endif
			printf("%s", "buffer: Cannot allocate buffer");
			b_free(temp_buf);
			return NULL;
	}
	//set remaining values 
	temp_buf->capacity = init_capacity;
	temp_buf->flags = DEFAULT_FLAGS;	
	return temp_buf;
}
/********************************************************
Purpose: Method used to add characters to the buffer
Author: Marco Gregory
History/Versions: 1.0
Called Functions: b_isfull(), b_incfactor()
Parameters: pBD , symbol: symbol to be added to the buffer 
Return Value: NULL: failure to add, pBuffer: successful addition of char
Algorithm:
	Resets the RFLAG
	If the buffer isn't full, adds a character to the next available space in cb_head and returns
	If the buffer is full, uses the mode to change buffer size.		
		additive mode: new capacity = capacity + inc factor
		fixed: no change, return null
		multiplicative: new capacity = available space * inc_factor / 100
	Allocates space for the new head and potentially update RFLAG
	Set new values such for cb_head, capacity, addc_offset, and add symbol
If at any point it fails, it returns NULL. If it succeeds, it returns pBD
*********************************************************/
pBuffer b_addc(pBuffer const pBD, char symbol) {
	char* temp = NULL; //container for a realloc operation
	long newsize = ZERO; //used to determine new size
	long newincrement = ZERO; //new inc_factor
	int availspace = ZERO; //available space remaining for capacity
	if (pBD) {
		pBD->flags &= RESET_R_FLAG;
		// if the buffer is not full, just add and return
		if (!(b_isfull(pBD))) { 
			pBD->cb_head[pBD->addc_offset] = symbol;
			if (inRange(ZERO, SHRT_MAX-ONE, pBD->addc_offset)) { 
				pBD->addc_offset++;
				return pBD;
			}
			else {
				#ifdef DEBUG
				printf("Buffer threatening to overflow pBD->addc_offset\n");
				#endif
				return NULL;
			}
		} 
		// buffer full; must resize buffer
		else {
			switch (pBD->mode) { //check operational mode 
				case ZERO:
					return NULL;
				case ONE: //newsize = capacity + incfactor
					newsize = pBD->capacity + (unsigned char)pBD->inc_factor * sizeof(char);
					if (!(inRange(ZERO, SHRT_MAX - ONE, newsize))) {
						return NULL;
					}
					break;
				case N_ONE: //newsize = formula presented in specs
					availspace = (SHRT_MAX - ONE) - pBD->capacity;
					if (!availspace) {
						#ifdef DEBUG		
						printf("Reached end of capacity. capacity = %u", pBD->capacity);
						#endif
						return NULL;
					}
					newincrement = availspace * b_incfactor(pBD) / MI_MAX;
					if (!(inRange(ONE, availspace, newincrement))) { // newincrement was evaluating to zero and causing buffer to overflow at last 6 characters. may be sve
						newincrement = availspace;
					}
					newsize = pBD->capacity + newincrement;
					if (!(inRange(ZERO, (SHRT_MAX - ONE), newsize))) {
						#ifdef DEBUG
						printf("No more space. Newsize = %u", SHRT_MAX-1);
						#endif
						newsize = SHRT_MAX - ONE;
					}
					break;
				default: 
					#ifdef DEBUG
					printf("Invalid operational mode.\n");
					#endif
					return NULL;
			}
			//make new head and potentially set RFLAG
			temp = (char*)realloc(pBD->cb_head, newsize);
			if (temp == NULL) {
			#ifdef DEBUG
				printf("Error reallocating memory for new characters\n");
			#endif  
				return NULL;
			}
			if (!(temp == pBD->cb_head)) {
				pBD->flags |= SET_R_FLAG;
			}
			//set remaining values
			pBD->cb_head = temp;
			pBD->capacity = (unsigned short) newsize; //now safe to cast 
			pBD->cb_head[pBD->addc_offset] = symbol;
			pBD->addc_offset++;
			return pBD;
		}
	}
	#ifdef DEBUG
	printf("Cannot add to uninitialized pointer\n");
	#endif 
	return NULL;
}
/********************************************************
Function name: b_clear
Purpose: clears the buffer in such a way that it appears as an empty new buffer. Does not actually clear all characters from the buffer.  
In parameters: pBD
Out parameters: ZERO, N_ONE
Version: 1.0
Author: Marco Gregory
*********************************************************/
int b_clear(Buffer* const pBD) {
	if (pBD) {
		//reset all neccessary values: 
		pBD->addc_offset = ZERO;
		pBD->getc_offset = ZERO;
		pBD->markc_offset = ZERO;
		return ZERO;
	}
	return RT_FAIL_1;
}
/********************************************************
Purpose: Frees memory allocated for buffer by b_allocate
Author: Marco Gregory
History/Versions: 1.0
Called Functions:
Parameters: pBD
Return Value:
*********************************************************/
void b_free(Buffer* const pBD) {
	if (pBD) {
		free(pBD->cb_head);
		free(pBD);
		return;
	}
	#ifdef DEBUG
	printf("Can not free an empty buffer*");
	#endif
}
/********************************************************
Purpose: Use to determine whether or not the buffer is full. 
Author: Marco Gregory
History/Versions: 1.0
Called Functions:
Parameters: pBD
Return Value: ONE: Buffer is full, ZERO: Buffer not full, RT_FAIL_1: invalid call
*********************************************************/
int b_isfull(Buffer * const pBD) {
	if (pBD) {
		if (pBD->capacity == (unsigned short)pBD->addc_offset) {
			return ONE;
		}
		return ZERO;
	}
	return RT_FAIL_1;
}
/********************************************************
Purpose: Use to determine index of next character to add to buffer
Author: Marco Gregory
History/Versions: 1.0
Called Functions:
Parameters: pBD
Return Value: short: index of addc_offset, RT_FAIL_1: invalid call
*********************************************************/
short b_limit(Buffer * const pBD) {
	if (pBD) {
		return pBD->addc_offset;
	}
	return RT_FAIL_1;
}
/********************************************************
Purpose: getter for pBD->capacity. 
Author: Marco Gregory
History/Versions: 1.0
Called Functions:
Parameters: pBD
Return Value: RT_FAIL_1: invalid call, short: capacity of buffer
*********************************************************/
short b_capacity(Buffer * const pBD) {
	if (pBD) {
		return pBD->capacity;
	}
	return RT_FAIL_1;
}
/********************************************************
Purpose: Setter for markc_offset value of buffer
Author: Marco Gregory
History/Versions: 1.0
Called Functions:
Parameters: pBD, mark(new markc_offset value): 0 to SHRT_MAX
Return Value: RT_FAIL_1: invalid call, short: pBD->markc_offset
*********************************************************/
short b_mark(Buffer * const pBD, short mark) {
	if (pBD) {
		if (inRange(ZERO, pBD->addc_offset, mark)) {
			pBD->markc_offset = mark;
			return pBD->markc_offset;
		}
		#ifdef DEBUG
		printf("Invalid mark range: %i is outside of range 0-%i\n", mark, pBD->addc_offset);
		#endif	
	}
	return RT_FAIL_1;
}
/********************************************************
Purpose: getter for pBD->mode. 
Author: Marco Gregory
History/Versions: 1.0
Called Functions:
Parameters: pBD
Return Value: RT_FAIL_2: invalid call, short:value of mode. Mode can only be O, 1, -1
*********************************************************/
int b_mode(Buffer * const pBD) {
	if (pBD) {
		return pBD->mode;
	}
	return RT_FAIL_2;
}
/********************************************************
Purpose: getter for inc_factor field of pBD.  
Author: Marco Gregory
History/Versions: 1.0
Called Functions:
Parameters: pBD
Return Value: RT_FAIL_1: invalid call, short: pBD->incfactor
*********************************************************/
size_t b_incfactor(Buffer * const pBD) {
	if (pBD) {
		return (unsigned char) pBD->inc_factor;
	}
	return RT_FAIL_4;
}
/********************************************************
Purpose: Load contents of the file pointed to by FILE* fi char by char into buffer* pBD
Author: Marco Gregory
History/Versions: 1.0
Called Functions:
Parameters: fi (file containing input), pBD
Return Value: RT_FAIL_1: invalid call, int: number of characters in file, LOAD_FAIL: failed to load
*********************************************************/
int b_load(FILE * const fi, Buffer * const pBD) {
	int temp=ZERO; //used to store characters from file
	int counter = ZERO; //count of characters in file
	pBuffer check = NULL; //error checker
	if (pBD) {
		while (ONE) { //This is broken when you reach eof
			temp = fgetc(fi);
			if (feof(fi)) {break;}
			check = b_addc(pBD, temp);
			//printf("addc: %u capacity: %u inc %u \n", pBD->addc_offset, pBD->capacity, pBD->inc_factor);
			if (check == NULL) {
				temp = ungetc(temp, fi);
				printf("The last character read from the file is: %c %i\n", temp, temp);
				return LOAD_FAIL;
			}
			counter++;
		}
		return counter;
	}
	return RT_FAIL_1;
}
/********************************************************
Purpose: Tells you whether or not pBD is empty
Author: Marco Gregory
History/Versions: 1.0
Called Functions:
Parameters: pBD
Return Value: RT_FAIL_1:invalid call, ZERO: buffer empty; ONE: buffer not empty.
*********************************************************/
int b_isempty(Buffer * const pBD) {
	if (pBD) {
		if (pBD->addc_offset == ZERO) {
			return ONE;
		}
		return ZERO;
	}
	return RT_FAIL_1;
}
/********************************************************
Purpose: Used in conjunction with b_eob to iterate through buffer. Gets char at the value of pBD->getc_offset. 
Example: while (!(b_eob(pBD))) {
			temp = b_getc(pBD);
			if (temp != RT_FAIL_2 && !(pBD->flags & CHECK_EOB)) {
				printf("%c", temp);
			}	
		}
Author: Marco Gregory
History/Versions: 1.0
Called Functions:
Parameters: pBD
Return Value: RT_FAIL_2: invalid call, ZERO: reached the end of the buffer. char: char at getc_offset
*********************************************************/
char b_getc(Buffer * const pBD) {
	char temp; //storage container for character to return
	if (pBD) {
		if (pBD->getc_offset == pBD->addc_offset) { //if buffer's full set eob and return 
			pBD->flags |= SET_EOB;
			return ZERO;
		}//buffer not full; return character
		pBD->flags &= RESET_EOB;
		temp = pBD->cb_head[pBD->getc_offset];
		pBD->getc_offset++;
		return temp; 
	}
	return RT_FAIL_2;
}
/********************************************************
Purpose: getter for EOB flag (bit 0 of pBD->flags)
Author: Marco Gregory
History/Versions: 1.0
Called Functions:
Parameters: pBD
Return Value: RT_FAIL_1: invalid call, short: value of EOB bit
*********************************************************/
int b_eob(Buffer * const pBD) {
	if (pBD) {
		if (pBD->flags & CHECK_EOB) {
			return ONE;
		}
		return ZERO;
	}
	return RT_FAIL_1;
}
/********************************************************
Purpose: Auxillary function to print values in buffer. 
Author: Marco Gregory
History/Versions: 1.0
Called Functions: b_getc(), b_isempty()
Parameters: pBD
Return Value: RT_FAIL_1: invalid call, ZERO: success
*********************************************************/
int b_print(Buffer* const pBD) {
	char temp = ZERO; //storage container for char returned by b_getc to print
	if (pBD && !(B_FULL(pBD))) {	
		//printf("Capacity: %i\nFlags %i\nMode:%i\n", b_capacity(pBD), pBD->flags, b_mode(pBD));
		while (!(B_FULL(pBD))) { //working example of b_full macro
			//get and print characters one by one with b_getc
			temp = b_getc(pBD);
			if (temp != RT_FAIL_2 && (!(B_FULL(pBD)))) {
				printf("%c", temp);
			}	
		}
		printf("\n");
		return ZERO;
	}
	printf("Empty buffer!\n");
	return RT_FAIL_1;
}
/********************************************************
Purpose: Compacts the memory space used by pBD->cb_head to be equal to the amount of characters currently residing in the buffer + 1. Adds character symbol to the end of the buffer. 
Author: Marco Gregory
History/Versions: 1.0
Called Functions:
Parameters: pBD, symbol(to add to the end of buffer)
Return Value: NULL: failed to compact, Buffer*: success
Algorithm:
	-Reset RFLAG
	-Reallocates cb_head of buffer
	-Checks for reallocation error
	-Potentially set RFLAG, set new cb_head, update some values
*********************************************************/
Buffer *b_compact(Buffer * const pBD, char symbol) {
	char* temp; //the address of the new pBD->cb_head
	if (pBD && inRange(ZERO, SHRT_MAX-ONE, pBD->addc_offset)) {
		pBD->flags &= RESET_R_FLAG;
		temp = (char*)realloc(pBD->cb_head, pBD->addc_offset + ONE); //compact buffer head to new size of addc_offset + 1
		if (temp == NULL) {
			#ifdef DEBUG
			printf("Error compacting buffer");
			#endif
			return NULL;
		} //if they compact to a bigger spot in memory, head address might change
		if (!(temp == pBD->cb_head)) {
			pBD->flags |= SET_R_FLAG;
			pBD->cb_head = temp;
		}
		//set remaining values
		pBD->cb_head[pBD->addc_offset] = symbol;
		pBD->addc_offset++;
		pBD->capacity = pBD->addc_offset;
		return pBD;
	}	
	return NULL;
}
/********************************************************
Purpose: Getter for RFLAG (bit 1 of pBD->flags). 
Author: Marco Gregory
History/Versions: 1.0
Called Functions:
Parameters: pBD
Return Value: RT_FAIL_1: invalid call, short: value of RFLAG 
*********************************************************/
char b_rflag(Buffer* const pBD) {
	if (pBD) {
		if (pBD->flags & CHECK_R_FLAG) { //if there is any product from mask&flag you have a one in the bit 
			return ONE;
		} else {
			return ZERO;
		}
	}
	return RT_FAIL_1;
}
/********************************************************
Purpose: Rewinds buffer by one character
Author: Marco Gregory
History/Versions: 1.0
Called Functions:
Parameters: pBD
Return Value: RT_FAIL_1: invalid call, short: decremented getc_offset  
*********************************************************/
short b_retract(Buffer* const pBD) { 
	if (pBD) {
		if (pBD->getc_offset > ZERO) {
			pBD->getc_offset--;
			return pBD->getc_offset;
		}
		//printf("Buffer cannot lower getc_offset below zero");
		return RT_FAIL_1;
	}
	return RT_FAIL_1;
}
/********************************************************
Purpose: Resets the getc_offset to the markc_offset
Author: Marco Gregory
History/Versions: 1.0
Called Functions:
Parameters: pBD
Return Value: RT_FAIL_1: invalid call, short: modified getc_offset
*********************************************************/
short b_reset(Buffer* const pBD) {
	if (pBD) {
		pBD->getc_offset = pBD->markc_offset;
		return pBD->getc_offset;
	}
	return RT_FAIL_1;
}
/********************************************************
Purpose: Getter for pBD->getc_offset
Author: Marco Gregory
History/Versions: 1.0
Called Functions:
Parameters: pBD
Return Value: RT_FAIL_1: invalid call, short: pbD->getcoffset
*********************************************************/
short b_getcoffset(Buffer* const pBD) {
	if (pBD) {
		return pBD->getc_offset;
	}
	return RT_FAIL_1;
}
/********************************************************
Purpose: Rewinds buffer. Used to reset getc and markc offset after reading buffer contents
Author: Marco Gregory
History/Versions: 1.0
Called Functions:
Parameters: pBD
Return Value: RT_FAIL_1: invalid call, ZERO:success
*********************************************************/
int b_rewind(Buffer* const pBD) {
	if (pBD) {
		pBD->flags &= RESET_EOB;
		pBD->getc_offset = ZERO;
		pBD->markc_offset = ZERO;
		return ZERO;
	}
	return RT_FAIL_1;
}
/********************************************************
Purpose: Getter for specific character in buffer
Author: Marco Gregory
History/Versions: 1.0
Called Functions:
Parameters: pBD
Return Value: NULL: loc_offset not between zero and b_limit, char*: address of the char at loc_offset on pBD->cb_head
*********************************************************/
char* b_location(Buffer* const pBD, short loc_offset) {
	if (pBD) {
		if (inRange(ONE, b_limit(pBD), loc_offset));
		{
			return &(pBD->cb_head[loc_offset]); //TODO: could be implemented more gracefully; solution is adequate
		}
	}
	return NULL;
}