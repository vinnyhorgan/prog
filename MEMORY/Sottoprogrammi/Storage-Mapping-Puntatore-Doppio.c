#include <stdio.h>
#include <stdlib.h>

void fun(int **mat, int righe, int colonne) {
    printf("Matrice:\n");
    for(int i = 0; i < righe; i++) {
        for(int j = 0; j < colonne; j++) {
            printf("%3d ", mat[i][j]);
        }
        printf("\n");
    }
}

int main() {
    // Metodo con allocazione dinamica frammentata
	//int prova[4][5];
    int **mat = malloc(4 * sizeof(int*));
    for(int i = 0; i < 4; i++) {
        mat[i] = malloc(5 * sizeof(int));
        for(int j = 0; j < 5; j++) {
            mat[i][j] = i * 5 + j + 1;
        }
    }
    //fun(prova, 4, 5);
    fun(mat, 4, 5);
    
    // Pulizia
    for(int i = 0; i < 4; i++) {
        free(mat[i]);
    }
    free(mat);
    
    return 0;
}