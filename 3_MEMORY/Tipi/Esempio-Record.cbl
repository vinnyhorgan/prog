IDENTIFICATION DIVISION.
       PROGRAM-ID. RECORD-DEMO.
       AUTHOR. Esempio Record COBOL.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
       *> Record per un Dipendente (struttura gerarchica)
       01  DIPENDENTE.
           05  MATRICOLA           PIC 9(6).
           05  DATI-ANAGRAFICI.
               10  COGNOME         PIC X(20).
               10  NOME            PIC X(20).
               10  DATA-NASCITA.
                   15  GIORNO      PIC 99.
                   15  MESE        PIC 99.
                   15  ANNO        PIC 9999.
           05  DATI-LAVORATIVI.
               10  DIPARTIMENTO    PIC X(15).
               10  STIPENDIO       PIC 9(5)V99.
               10  ANNI-SERVIZIO   PIC 99.
       
       *> Record per un Prodotto
       01  PRODOTTO.
           05  CODICE-PROD         PIC X(10).
           05  DESCRIZIONE         PIC X(30).
           05  PREZZO              PIC 9(4)V99.
           05  QUANTITA            PIC 9(5).
           05  VALORE-TOTALE       PIC 9(7)V99.
       
       *> Variabili di supporto
       01  AUMENTO                 PIC 9(4)V99.
       01  NUOVO-STIPENDIO         PIC 9(5)V99.
       
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           PERFORM INIZIALIZZA-DIPENDENTE
           PERFORM MOSTRA-DIPENDENTE
           PERFORM AGGIORNA-STIPENDIO
           
           DISPLAY " "
           DISPLAY "================================"
           DISPLAY " "
           
           PERFORM INIZIALIZZA-PRODOTTO
           PERFORM MOSTRA-PRODOTTO
           PERFORM CALCOLA-VALORE
           
           STOP RUN.
       
       INIZIALIZZA-DIPENDENTE.
           MOVE 123456 TO MATRICOLA
           MOVE "ROSSI" TO COGNOME
           MOVE "MARIO" TO NOME
           MOVE 15 TO GIORNO
           MOVE 03 TO MESE
           MOVE 1985 TO ANNO
           MOVE "INFORMATICA" TO DIPARTIMENTO
           MOVE 35000.00 TO STIPENDIO
           MOVE 10 TO ANNI-SERVIZIO.
       
       MOSTRA-DIPENDENTE.
           DISPLAY "=== DATI DIPENDENTE ==="
           DISPLAY "Matricola: " MATRICOLA
           DISPLAY "Nome completo: " NOME " " COGNOME
           DISPLAY "Data di nascita: " GIORNO "/" MESE "/" ANNO
           DISPLAY "Dipartimento: " DIPARTIMENTO
           DISPLAY "Stipendio: EUR " STIPENDIO
           DISPLAY "Anni di servizio: " ANNI-SERVIZIO.
       
       AGGIORNA-STIPENDIO.
           *> Calcola aumento del 5%
           COMPUTE AUMENTO = STIPENDIO * 0.05
           COMPUTE NUOVO-STIPENDIO = STIPENDIO + AUMENTO
           MOVE NUOVO-STIPENDIO TO STIPENDIO
           
           DISPLAY " "
           DISPLAY "Aumento del 5% applicato"
           DISPLAY "Nuovo stipendio: EUR " STIPENDIO.
       
       INIZIALIZZA-PRODOTTO.
           MOVE "PROD-001" TO CODICE-PROD
           MOVE "Laptop Dell XPS 15" TO DESCRIZIONE
           MOVE 1299.99 TO PREZZO
           MOVE 25 TO QUANTITA.
       
       MOSTRA-PRODOTTO.
           DISPLAY "=== DATI PRODOTTO ==="
           DISPLAY "Codice: " CODICE-PROD
           DISPLAY "Descrizione: " DESCRIZIONE
           DISPLAY "Prezzo unitario: EUR " PREZZO
           DISPLAY "Quantita' in magazzino: " QUANTITA.
       
       CALCOLA-VALORE.
           COMPUTE VALORE-TOTALE = PREZZO * QUANTITA
           DISPLAY "Valore totale magazzino: EUR " VALORE-TOTALE.