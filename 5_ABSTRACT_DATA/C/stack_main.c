#include <stdio.h>
#include <stdlib.h>

#include "stack.h"

int main(int argc, char ** argv) {
Stack s;
int i;

	if (argc <= 1) return -1;

	create(&s);
	
	for ( i = 0; i<atoi(argv[1]); i++) push(&s, i);

	while ( empty(s) == 0 ) {
		printf("Stack top is: %d\n" , top(s) );
		pop(&s);
	}

	destroy(&s);

	return 0;
}
