#include <stdio.h>

/*void funzione(int mat[][]) {  // ERRORE!
    // Come faccio a calcolare mat[i][j]?
}*/

/*void funzione(int mat[][5], int righe) {
    // Funziona: il compilatore sa che N=5
    int x = mat[2][3];  // OK!
}*/

void funzione(int **mat, int righe, int colonne) {
    int x = mat[2][3];  // OK, ma la matrice potrebbe essere frammentata
}

// Metodo 1: dimensione colonne fissa
void stampa1(int mat[][3], int righe) {
    for(int i = 0; i < righe; i++) {
        for(int j = 0; j < 3; j++) {
            printf("%d ", mat[i][j]);
        }
        printf("\n");
    }
}

// Metodo 2: puntatore e dimensioni variabili
void stampa2(int *mat, int righe, int colonne) {
    for(int i = 0; i < righe; i++) {
        for(int j = 0; j < colonne; j++) {
            printf("%d ", mat[i * colonne + j]);
        }
        printf("\n");
    }
}

int main() {
    int matrice[2][3] = {{1,2,3}, {4,5,6}};
    
    stampa1(matrice, 2);
    stampa2(&matrice[0][0], 2, 3);
    
    return 0;
}