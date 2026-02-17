#include <stdio.h>

// Funzioni che operano su due interi
int somma(int a, int b) {
    return a + b;
}

int sottrazione(int a, int b) {
    return a - b;
}

int moltiplicazione(int a, int b) {
    return a * b;
}

// Funzione che accetta un puntatore a funzione come parametro
int eseguiOperazione(int x, int y, int (*operazione)(int, int)) {
    return operazione(x, y);
}

int main() {
    // Dichiarazione di un puntatore a funzione
    // Sintassi: tipo_ritorno (*nome_puntatore)(parametri)
    int (*ptrFunzione)(int, int);
    
    // Assegnazione del puntatore alla funzione somma
    ptrFunzione = somma;
    printf("Somma: %d\n", ptrFunzione(5, 3));  // Output: 8
    
    // Riassegnazione a un'altra funzione
    ptrFunzione = sottrazione;
    printf("Sottrazione: %d\n", ptrFunzione(5, 3));  // Output: 2
    
    ptrFunzione = moltiplicazione;
    printf("Moltiplicazione: %d\n", ptrFunzione(5, 3));  // Output: 15
    
    // Uso di callback - passare funzione come parametro
    printf("\nUsando callback:\n");
    printf("5 + 3 = %d\n", eseguiOperazione(5, 3, somma));
    printf("5 - 3 = %d\n", eseguiOperazione(5, 3, sottrazione));
    printf("5 * 3 = %d\n", eseguiOperazione(5, 3, moltiplicazione));
    
    return 0;
}