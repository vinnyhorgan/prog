#include <stdio.h>
#include <stdlib.h>

// STATIC: variabile globale/static - allocata a compile-time
static int counter = 0;

void esempio_c() {
    // STACK-DYNAMIC: variabile locale - allocata automaticamente
    int stack_var = 42;
    
    // HEAP-DYNAMIC EXPLICIT: allocazione manuale con malloc
    int* heap_var = (int*)malloc(sizeof(int));
    *heap_var = 100;
    
    counter++;
    
    printf("C - Static: %d, Stack: %d, Heap: %d\n", 
           counter, stack_var, *heap_var);
    
    // Deallocazione esplicita necessaria
    free(heap_var);
}

int main() {
    esempio_c();
    esempio_c();
    return 0;
}