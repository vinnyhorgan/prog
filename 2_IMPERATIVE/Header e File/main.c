/* main.c - Programma principale */
#include <stdio.h>
#include "config.h"

int main(void) {
    printf("=== Esempio Compilazione Separata ===\n\n");
    
    /* Uso della macro condizionale */
    #ifdef DEBUG_MODE
    printf("Modalità DEBUG attiva!\n");
    #endif
    
    LOG("Inizio programma");
    
    /* Uso della variabile extern */
    printf("\nValore iniziale global_counter: %d\n", global_counter);
    
    /* Chiamata a funzione esterna */
    print_config();
    
    /* Incremento del counter */
    printf("\nIncremento counter...\n");
    increment_counter();
    increment_counter();
    
    printf("Nuovo valore global_counter: %d\n", global_counter);
    
    /* Ogni file che include config.h ha la sua copia di header_static_var */
    printf("\nHeader static var in main.c: %d\n", header_static_var);
    header_static_var = 99;
    printf("Modifico header_static_var in main.c: %d\n", header_static_var);
    
    /* La copia in utils.c rimane invariata */
    printf("\nChiamo print_config() da utils.c:\n");
    print_config();
    
    /* ERRORE: non posso chiamare private_function() o accedere a private_counter
     * perché sono static in utils.c */
    // private_function();  // ERRORE di compilazione!
    // printf("%d", private_counter);  // ERRORE di compilazione!
    
    return 0;
}