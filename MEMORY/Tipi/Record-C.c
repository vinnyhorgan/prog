#include <stdio.h>
#include <string.h>

// Struttura per la data di nascita
typedef struct {
    int giorno;
    int mese;
    int anno;
} DataNascita;

// Struttura per i dati anagrafici
typedef struct {
    char cognome[21];
    char nome[21];
    DataNascita data_nascita;
} DatiAnagrafici;

// Struttura per i dati lavorativi
typedef struct {
    char dipartimento[16];
    float stipendio;
    int anni_servizio;
} DatiLavorativi;

// Struttura completa Dipendente
typedef struct {
    int matricola;
    DatiAnagrafici anagrafici;
    DatiLavorativi lavorativi;
} Dipendente;

// Struttura Prodotto
typedef struct {
    char codice_prod[11];
    char descrizione[31];
    float prezzo;
    int quantita;
    float valore_totale;
} Prodotto;

void inizializza_dipendente(Dipendente *dip) {
    dip->matricola = 123456;
    strcpy(dip->anagrafici.cognome, "ROSSI");
    strcpy(dip->anagrafici.nome, "MARIO");
    dip->anagrafici.data_nascita.giorno = 15;
    dip->anagrafici.data_nascita.mese = 3;
    dip->anagrafici.data_nascita.anno = 1985;
    strcpy(dip->lavorativi.dipartimento, "INFORMATICA");
    dip->lavorativi.stipendio = 35000.00;
    dip->lavorativi.anni_servizio = 10;
}

void mostra_dipendente(Dipendente *dip) {
    printf("=== DATI DIPENDENTE ===\n");
    printf("Matricola: %d\n", dip->matricola);
    printf("Nome completo: %s %s\n", dip->anagrafici.nome, dip->anagrafici.cognome);
    printf("Data di nascita: %02d/%02d/%d\n", 
           dip->anagrafici.data_nascita.giorno,
           dip->anagrafici.data_nascita.mese,
           dip->anagrafici.data_nascita.anno);
    printf("Dipartimento: %s\n", dip->lavorativi.dipartimento);
    printf("Stipendio: EUR %.2f\n", dip->lavorativi.stipendio);
    printf("Anni di servizio: %d\n", dip->lavorativi.anni_servizio);
}

void aggiorna_stipendio(Dipendente *dip) {
    float aumento = dip->lavorativi.stipendio * 0.05;
    dip->lavorativi.stipendio += aumento;
    
    printf("\n");
    printf("Aumento del 5%% applicato\n");
    printf("Nuovo stipendio: EUR %.2f\n", dip->lavorativi.stipendio);
}

void inizializza_prodotto(Prodotto *prod) {
    strcpy(prod->codice_prod, "PROD-001");
    strcpy(prod->descrizione, "Laptop Dell XPS 15");
    prod->prezzo = 1299.99;
    prod->quantita = 25;
    prod->valore_totale = 0.0;
}

void mostra_prodotto(Prodotto *prod) {
    printf("=== DATI PRODOTTO ===\n");
    printf("Codice: %s\n", prod->codice_prod);
    printf("Descrizione: %s\n", prod->descrizione);
    printf("Prezzo unitario: EUR %.2f\n", prod->prezzo);
    printf("Quantita' in magazzino: %d\n", prod->quantita);
}

void calcola_valore(Prodotto *prod) {
    prod->valore_totale = prod->prezzo * prod->quantita;
    printf("Valore totale magazzino: EUR %.2f\n", prod->valore_totale);
}

int main() {
    Dipendente dipendente;
    Prodotto prodotto;
    
    inizializza_dipendente(&dipendente);
    mostra_dipendente(&dipendente);
    aggiorna_stipendio(&dipendente);
    
    printf("\n");
    printf("================================\n");
    printf("\n");
    
    inizializza_prodotto(&prodotto);
    mostra_prodotto(&prodotto);
    calcola_valore(&prodotto);
    
    return 0;
}