#include <stdio.h>

int somma(int a, int b) {
    return a + b;
}

float divisioneFloat(float a, float b) {
    return a / b;
}

void stampa(char* messaggio) {
    printf("%s\n", messaggio);
}

int main() {
    // Passare il tipo sbagliato di puntatore a funzione
    void calcolaGenerico(int (*f)(int, int)) {
        printf("%d\n", f(10, 5));
    }
    
    // Cast che maschera il tipo reale
    calcolaGenerico((int (*)(int, int))divisioneFloat);
    // Comportamento indefinito - interpreta float come int
    
    return 0;
}