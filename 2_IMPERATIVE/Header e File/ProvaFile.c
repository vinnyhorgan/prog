#include <stdio.h>
#include <stdlib.h>

int main() {
    FILE *file_input, *file_output;
    char carattere;
    int contatore = 0;
    
    // === SCRITTURA SU FILE ===
    printf("=== SCRITTURA SU FILE ===\n");
    
    // Apre file in modalità scrittura ("w" = write)
    file_output = fopen("output.txt", "w");
    
    if (file_output == NULL) {
        printf("Errore nell'apertura del file output.txt\n");
        return 1;
    }
    
    // Scrive caratteri sul file
    fprintf(file_output, "Ciao mondo!\n");
    fprintf(file_output, "Questa e' la seconda riga.\n");
    fprintf(file_output, "Numeri: 123\n");
    
    // Scrive caratteri singoli
    fputc('A', file_output);
    fputc('B', file_output);
    fputc('C', file_output);
    fputc('\n', file_output);
    
    fclose(file_output);
    printf("File 'output.txt' scritto con successo!\n\n");
    
    
    // === LETTURA DA FILE ===
    printf("=== LETTURA DA FILE ===\n");
    
    // Apre file in modalita' lettura ("r" = read)
    file_input = fopen("output.txt", "r");
    
    if (file_input == NULL) {
        printf("Errore nell'apertura del file output.txt\n");
        return 1;
    }
    
    printf("Contenuto del file:\n");
    printf("-------------------\n");
    
    // Legge carattere per carattere fino alla fine del file
    while ((carattere = fgetc(file_input)) != EOF) {
        //putchar(carattere);  // Stampa su schermo
		putc(carattere, stdout);
        contatore++;
    }
    
    printf("-------------------\n");
    printf("Caratteri letti: %d\n\n", contatore);
    
    fclose(file_input);
    
    
    // === APPEND (AGGIUNTA) AL FILE ===
    printf("=== AGGIUNTA AL FILE ===\n");
    
    // Apre file in modalità append ("a" = append)
    file_output = fopen("output.txt", "a");
    
    if (file_output == NULL) {
        printf("Errore nell'apertura del file in modalità append\n");
        return 1;
    }
    
    fprintf(file_output, "Questa riga e' stata aggiunta dopo!\n");
    fclose(file_output);
    printf("Riga aggiunta al file!\n\n");
    
    
    // === LETTURA CON FGETS (righe intere) ===
    printf("=== LETTURA CON FGETS ===\n");
    
    file_input = fopen("output.txt", "r");
    char buffer[256];
    int num_riga = 1;
    
    printf("Lettura riga per riga:\n");
    while (fgets(buffer, sizeof(buffer), file_input) != NULL) {
        printf("Riga %d: %s", num_riga, buffer);
        num_riga++;
    }
    
    fclose(file_input);
    
    return 0;
}