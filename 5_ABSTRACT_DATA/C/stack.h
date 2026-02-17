#define MAX_STACK_SIZE 100

typedef char element;

struct my_elementstack_struct {

	element * v;
	int p;
	
};

typedef struct my_elementstack_struct Stack;

void create(Stack * s);
void destroy(Stack * s);
int empty(Stack s);
void push(Stack * s, element elem);
void pop(Stack * s);
element top(Stack s);
