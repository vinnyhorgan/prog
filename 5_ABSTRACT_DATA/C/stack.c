#include <stdlib.h>
#include "stack.h"

void create(Stack * s) {
	(*s).v = (element *) malloc (MAX_STACK_SIZE * sizeof(element) );
	(*s).p = 0;
}

void destroy(Stack * s) {
	free( (*s).v );
	(*s).p = -1;
}

int empty(Stack s) {
	return ( s.p == 0 );
}

void push(Stack * s, element elem) {
	if ( (*s).p < MAX_STACK_SIZE )
		(*s).v[ (*s).p++ ] = elem;
}

void pop(Stack * s) {
	if ( (*s).p > 0 )
		(*s).p--;
}

element top(Stack s) {
	if ( s.p > 0 )
		return s.v[ s.p - 1 ];
	else
		return -1;
}
