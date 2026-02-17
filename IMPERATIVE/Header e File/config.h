/* config.h - Header con macro e configurazioni */
#ifndef CONFIG_H
#define CONFIG_H

/* Esempio di macro condizionale per definire una costante */
#ifdef DEBUG_MODE
    #define MAX_SIZE 10
    #define LOG(msg) printf("DEBUG: %s\n", msg)
#else
    #define MAX_SIZE 100
    #define LOG(msg) /* niente */
#endif

/* Costante definita con macro */
#define PI 3.14159

/* ERRORE COMUNE: Definire una variabile in un header (NON FARE!)
 * Questo causa errori di "multiple definition" in compilazione separata */
// int global_counter = 0;  // SBAGLIATO!

/* CORRETTO: Dichiarazione extern in header, definizione nel .c */
extern int global_counter=0;

/* Variabile statica in header - ogni file che include questo header 
 * avrà la sua copia privata (generalmente da evitare negli header) */
static int header_static_var = 42;

/* Dichiarazione di funzione esterna */
extern void print_config(void);
extern int increment_counter(void);

#endif /* CONFIG_H */