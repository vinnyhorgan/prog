/* utils.c - Implementazione delle funzioni */
#include <stdio.h>
#include "config.h"

/* Definizione della variabile globale dichiarata in config.h */
int global_counter = 0;

/* Variabile statica - visibile SOLO in questo file */
static int private_counter = 0;

/* Funzione esterna - può essere chiamata da altri file */
void print_config(void) {
    printf("=== Configurazione ===\n");
    printf("MAX_SIZE: %d\n", MAX_SIZE);
    printf("PI: %.5f\n", PI);
    printf("Global counter: %d\n", global_counter);
    printf("Private counter: %d\n", private_counter);
    printf("Header static var: %d\n", header_static_var);
}

/* Funzione esterna */
int increment_counter(void) {
    global_counter++;
    private_counter++;
    return global_counter;
}

/* Funzione statica - visibile SOLO in questo file */
static void private_function(void) {
    printf("Questa funzione è privata a utils.c\n");
}

/* Funzione che usa la funzione privata */
void use_private(void) {
    private_function();
}