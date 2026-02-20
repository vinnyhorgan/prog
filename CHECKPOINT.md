# Linguaggi di Programmazione - Sezioni 1, 2, 3

Questo documento riassume i contenuti delle prime tre sezioni del corso di Linguaggi di Programmazione, basato sul libro "Concepts of Programming Languages" (12th Edition) di Robert W. Sebesta.

---

## SEZIONE 1: INTRODUZIONE AL CORSO (1_INTRO)

### Argomenti Trattati

- Organizzazione del corso, regolamento, modalità d'esame
- Introduzione ai linguaggi di programmazione: perché studiarli, come classificarli e valutarli

### Riferimenti Bibliografici
- Sebesta, Capitoli 1 e 2

### 1.1 Perché Studiare i Concetti dei Linguaggi di Programmazione

Studiare i concetti dei linguaggi di programmazione offre numerosi benefici:

1. **Aumento della capacità di esprimere idee** - La profondità del pensiero è influenzata dal potere espressivo del linguaggio. Conoscere costrutti diversi permette di superare limitazioni nello sviluppo software.

2. **Migliore capacità di scegliere linguaggi appropriati** - Programmatori che conoscono un'ampia gamma di linguaggi possono scegliere lo strumento più adatto per il problema specifico.

3. **Maggiore capacità di imparare nuovi linguaggi** - Una comprensione approfondita dei concetti fondamentali rende più facile apprendere nuovi linguaggi.

4. **Migliore comprensione dell'implementazione** - Conoscere i dettagli di implementazione aiuta a usare un linguaggio in modo più intelligente.

5. **Migliore utilizzo dei linguaggi già conosciuti** - Studiare i concetti permette di scoprire e usare parti del linguaggio precedentemente ignorate.

### 1.2 Domini di Programmazione

| Dominio | Caratteristiche | Linguaggi Tipici |
|---------|-----------------|------------------|
| **Applicazioni Scientifiche** | Strutture dati semplici, molte operazioni in virgola mobile | Fortran, ALGOL 60 |
| **Applicazioni Business** | Report elaborati, decimali precisi, aritmetica decimale | COBOL |
| **Intelligenza Artificiale** | Computazione simbolica, liste concatenate, flessibilità | Lisp, Prolog, Python |
| **Software Web** | Diverse tecnologie, scripting, markup | JavaScript, PHP, Java |

### 1.3 Criteri di Valutazione dei Linguaggi

#### 1.3.1 Leggibilità (Readability)
Prima del 1970 l'efficienza era il criterio principale. Dagli anni '70 la manutenibilità del software ha assunto un ruolo fondamentale.

**Caratteristiche che influenzano la leggibilità:**

- **Semplicità complessiva**: Un linguaggio con troppe costruzioni di base è difficile da imparare.
- **Molleplicità di funzioni**: Avere più modi per fare la stessa operazione (es. `count++`, `++count`, `count += 1`) riduce la leggibilità.
- **Overloading degli operatori**: Un singolo simbolo con più significati può confondere.

- **Ortogonalità**: Un insieme relativamente piccolo di costrutti primitivi può essere combinato in un numero relativamente piccolo di modi. Ogni combinazione possibile è legale e significativa.
  - Esempio di mancanza di ortogonalità in C: gli array non possono essere restituiti dalle funzioni, mentre le strutture sì.

- **Tipi di Dato Adeguati**: La presenza di strutture dati appropriate aiuta la leggibilità (es. tipo Boolean vs usare numeri per flag).

- **Design della Sintassi**: Forma delle parole speciali (`if`, `while`, `class`), forme di chiusura dei costrutti.

#### 1.3.2 Scrivibilità (Writability)
La facilità con cui un linguaggio può essere usato per creare programmi.

- **Semplicità e Ortogonalità**: Un linguaggio con molti costrutti diversi è più difficile da usare. Tuttavia troppa ortogonalità può causare problemi (es. ALGOL 68 permette costrutti eccessivamente complessi).
- **Espressività**: Avere operatori potenti che permettono di fare molto con poco codice (es. `count++` vs `count = count + 1`).

#### 1.3.3 Affidabilità (Reliability)
Un programma è affidabile se si comporta secondo le specifiche in tutte le condizioni.

- **Checking dei Tipi**: Rilevamento di errori di tipo.
- **Handling delle Eccezioni**: Intercettazione e gestione di errori a runtime.
- **Aliasing**: Presenza di più nomi per lo stesso oggetto in memoria.

### 1.4 Influenze sul Design dei Linguaggi

**Architettura della Macchina**: Il linguaggio von Neumann (memoria centrale, unità di elaborazione) ha influenzato fortemente i linguaggi imperativi.

**Metodologie di Progettazione**: Dalla programmazione strutturata (anni '70) alla programmazione orientata agli oggetti (anni '80-'90).

### 1.5 Categorie di Linguaggi

| Categoria | Descrizione | Esempi |
|-----------|-------------|--------|
| **Imperativi** | Variabili, assegnamenti, iterazioni | C, Java, Python |
| **Funzionali** | Chiamate di funzioni senza side effects | Lisp, Scheme, Haskell |
| **Logici** | Logica delle regole, inferenza | Prolog |
| **Orientati agli Oggetti** | Incapsulamento, ereditarietà, polimorfismo | Java, C++, Smalltalk |
| **Scripting** | Interpretati, alta produttività | JavaScript, PHP, Python |
| **Markup/ibridi** | Marcatura + capacità computazionali | HTML, XML, JSP |

### 1.6 Metodi di Implementazione

- **Compilazione**: Traduzione completa in codice macchina prima dell'esecuzione (C, C++, Java compilato).
- **Interpretazione Pura**: Nessuna traduzione, esecuzione diretta del sorgente (es. primi interpreti BASIC).
- **Approcci Ibridi**: Compilazione a bytecode + interpretazione (JVM per Java, .NET per C#).

### 1.7 Slide di Riferimento
- [Introduzione alle tipologie di linguaggi](https://elearning.unimib.it/pluginfile.php/2004422/mod_resource/content/1/Intro.html)
- [Evoluzione dei Linguaggi di Programmazione](https://elearning.unimib.it/pluginfile.php/2000208/mod_resource/content/1/Linguaggi%20di%20Programmazione%20-%20Evoluzione%20dei%20Linguaggi%20%281%29.html)
- [Linguaggi di Programmazione: Definizione e Aspetti Fondamentali](https://elearning.unimib.it/pluginfile.php/2000212/mod_resource/content/1/sintassi_semantica_presentation.html)

---

## SEZIONE 2: LINGUAGGI IMPERATIVI (2_IMPERATIVE)

### Argomenti Trattati

- Compilazione, tipi fondamentali e primo programma
- L'ecosistema del Linguaggio C
- Direttive, costanti e Input/output

### 2.1 Panoramica Storica dei Linguaggi

Evoluzione dei principali linguaggi di programmazione:

| Linguaggio | Anno | Caratteristiche |
|------------|------|-----------------|
| **Plankalkül** (Zuse) | 1942-45 | Primo linguaggio ad alto livello, mai implementato |
| **Fortran** | 1957 | Primo linguaggio scientifico di successo, efficienza |
| **Lisp** | 1959 | Primo linguaggio funzionale, liste, garbage collection |
| **COBOL** | 1960 | Linguaggio business, report, aritmetica decimale |
| **ALGOL 60** | 1960 | Struttura a blocchi, ricorsione, begin-end |
| **BASIC** | 1964 | Semplice, per principianti, timesharing |
| **PL/I** | 1964 | Tentativo di essere "tutto per tutti" |
| **SIMULA 67** | 1967 | Primo linguaggio ad oggetti, classi |
| **ALGOL 68** | 1968 | Estrema ortogonalità, complessità |
| **C** | 1972 | Sistema, basso livello, portabile |
| **Smalltalk** | 1972 | Puramente orientato agli oggetti |
| **C++** | 1985 | C + classi + template |
| **Java** | 1995 | Portabile, bytecode, JVM, OOP |
| **C#** | 2000 | .NET, ibrido Microsoft |
| **Scripting** | 1990s-2000s | Perl, Python, Ruby, JavaScript, PHP |

### 2.2 L'Ecosistema del Linguaggio C

Il linguaggio C (creato da Dennis Ritchie, 1972) è caratterizzato da:

- **Tipi fondamentali**: `int`, `float`, `double`, `char`, `void`
- **Controllo basso livello**: Puntatori, gestione manuale della memoria
- **Efficienza**: Codice vicino alla macchina
- **Portabilità**: Compilatore disponibile su quasi tutte le piattaforme
- **Compilazione separata**: Supporto per progetti multi-file

### 2.3 Elementi Base del C

#### Tipi di Dato
```c
int a[] = {1, 2, 3, 4};        // Array di interi
char stringa[] = {'D', 'y', 'l', 'a', 'n'};  // Array di caratteri
int x = 42;                     // Intero
int* p = &x;                   // Puntatore a intero
int* q = p;                    // Alias del puntatore
int y = *q;                    // Dereferenziazione
int *z = (int*) malloc(10 * sizeof(int));  // Allocazione dinamica
```

#### Compilazione Separata
Il C supporta la compilazione modulare attraverso:
- **File header (.h)**: Dichiarazioni, macro, prototipi
- **File sorgente (.c)**: Implementazioni
- **Direttiva `#include`**: Inclusione degli header
- **Include guards**: `#ifndef`, `#define`, `#endif`

### 2.4 Slide di Riferimento
- [Introduzione: compilazione, tipi fondamentali e primo programma](https://elearning.unimib.it/pluginfile.php/2008439/mod_resource/content/2/c_cpp_presentation_1.html)
- [L'ecosistema del Linguaggio C](https://elearning.unimib.it/pluginfile.php/2008441/mod_resource/content/1/c_ecosystem_presentation.html)
- [Direttive, costanti e Input/output](https://elearning.unimib.it/pluginfile.php/2008448/mod_resource/content/1/cpp_io_presentation.html)

---

## SEZIONE 3: MEMORIA, SCOPE E TIPI (3_MEMORY)

### Argomenti Trattati

- Attributi di una variabile
- Binding: concetto, binding time, storage binding, lifetime
- Scoping: sottoprogrammi e record di attivazione, scoping statico e dinamico
- Passaggio di parametri a sottoprogrammi
- Controlli di tipo, tipizzazione statica/dinamica/implicita/esplicita
- Conversioni e inferenza di tipo
- Tipizzazione forte vs debole, linguaggi type safe vs unsafe
- Equivalenza e compatibilità tra tipi
- Esempi di type system

### Riferimenti Bibliografici
- Sebesta, 5.1-5.8, 6, 9.4, 9.5, 10.1, 10.2

### 3.1 Attributi delle Variabili

Ogni variabile ha sei attributi fondamentali:

| Attributo | Descrizione |
|-----------|-------------|
| **Nome** | Identificatore simbolico |
| **Indirizzo** | Locazione di memoria (l-value) |
| **Valore** | Contenuto attuale (r-value) |
| **Tipo** | Insieme di valori possibili e operazioni |
| **Lifetime** | Periodo durante cui esiste in memoria |
| **Scope** | Regione del programma dove è visibile |

### 3.2 Il Concetto di Binding

**Binding**: Associazione tra un attributo e un'entità di programma.

**Binding Time**: Momento in cui avviene l'associazione:
- **Compile-time**: Tipo, scope statico
- **Load-time**: Variabili globali statiche
- **Runtime**: Valori, allocazioni dinamiche

### 3.3 Storage Binding e Lifetime

#### Categorie di Variabili per Lifetime

| Categoria | Allocazione | Deallocazione | Esempio |
|-----------|-------------|---------------|---------|
| **Static** | Prima dell'esecuzione | Mai | Variabili globali C (`static`) |
| **Stack-dynamic** | All'ingresso del blocco | All'uscita del blocco | Variabili locali C/Java |
| **Heap-dynamic esplicito** | Su richiesta (malloc/new) | Su richiesta (free/delete) | Puntatori C, oggetti C++ |
| **Heap-dynamic implicito** | Su assegnazione | Garbage collection | Oggetti Java, Python |

#### Esempi di Allocazione

**C - Allocazione esplicita:**
```c
static int counter = 0;        // STATIC: allocata a compile-time

void esempio() {
    int stack_var = 42;       // STACK-DYNAMIC
    int* heap_var = (int*)malloc(sizeof(int));  // HEAP esplicito
    *heap_var = 100;
    free(heap_var);           // Deallocazione manuale
}
```

**Java - Garbage Collection automatica:**
```java
private static int counter = 0;  // STATIC

public static void mostra() {
    int stackVar = 42;              // STACK
    Integer heapVar = new Integer(100);  // HEAP implicito
    StringBuilder builder = new StringBuilder("heap");
    // GC si occupa automaticamente della deallocazione
}
```

### 3.4 Scope

**Scope**: La regione del programma dove una variabile è visibile.

#### Scoping Statico (Lexical Scoping)
Il scope è determinato dalla struttura lessicale del programma. La risoluzione dei nomi avviene basandosi sul testo del programma.

**Esempio in Pascal:**
```pascal
program ScopeStaticoEsempio;

procedure A;
var x: integer;
    
    procedure B;
    var y: integer;
        
        procedure C;
        begin
            writeln('Valore di x in C: ', x);  // Accede a x di A
            writeln('Valore di y in C: ', y);  // Accede a y di B
        end;
        
    begin
        y := 20;
        C();
    end;
    
begin
    x := 10;
    B();
end;
```

#### Scoping Dinamico
Il scope dipende dalla sequenza di chiamate a runtime. Una variabile è visibile se esiste nell'ambiente chiamante corrente.

**Esempio in Perl:**
```perl
sub A {
    my $x = 10;
    B();  # Chiama B
}

sub B {
    # In scoping dinamico, potrebbe vedere $x di A
    print $x;
}
```

### 3.5 Record di Attivazione (Activation Record)

Struttura dati in stack che contiene informazioni per ogni chiamata di funzione:

| Campo | Descrizione |
|-------|-------------|
| **Parametri formali** | Valori passati |
| **Indirizzo di ritorno** | Dove tornare dopo la chiamata |
| **Link dinamico** | Puntatore al record precedente nello stack |
| **Link statico** | Puntatore al record dello scope genitore |
| **Variabili locali** | Dati locali della funzione |
| **Temporanei** | Valori intermedi |

### 3.6 Passaggio dei Parametri

| Metodo | Descrizione | Implementazione |
|--------|-------------|-----------------|
| **Pass-by-value** | Copia del valore | Parametro = variabile locale inizializzata |
| **Pass-by-result** | Copia del risultato | Valore finale copiato nell'argomento |
| **Pass-by-value-result** | Entrambi (in-out) | Copia in entrata e in uscita |
| **Pass-by-reference** | Alias (indirizzo) | Parametro = puntatore all'argomento |
| **Pass-by-name** | Sostituzione testuale | Macro espansa ogni uso |

### 3.7 Sistemi di Tipi (Type Systems)

#### Classificazione della Tipizzazione

| Caratteristica | Descrizione | Esempi |
|----------------|-------------|--------|
| **Statica** | Tipi controllati a compile-time | C, Java, Pascal |
| **Dinamica** | Tipi controllati a runtime | Python, JavaScript, Ruby, Lisp |
| **Implicita** | Tipo inferito dal compilatore | ML, Haskell, Python (dichiarazione) |
| **Esplicita** | Tipo dichiarato dal programmatore | C, Java, Pascal |
| **Forte** | Nessuna conversione implicita pericolosa | Java, Python, ML |
| **Debole** | Conversioni automatiche permissive | C, C++, JavaScript |
| **Type Safe** | Nessun accesso non autorizzato alla memoria | Java, Python, C# |
| **Unsafe** | Possibile accesso arbitrario alla memoria | C, C++ (con puntatori) |

#### Equivalenza dei Tipi

- **Equivalenza per nome**: Due tipi sono uguali solo se hanno lo stesso nome.
- **Equivalenza strutturale**: Due tipi sono uguali se hanno la stessa struttura.

#### Compatibilità dei Tipi

Determina se un valore di tipo T1 può essere assegnato a una variabile di tipo T2:
- **Compabilità per assegnazione**: T1 è compatibile con T2
- **Coercizione**: Conversione implicita permessa

### 3.8 Record (Struct)

Strutture dati che raggruppano elementi eterogenei.

#### Record in C
```c
typedef struct {
    int giorno;
    int mese;
    int anno;
} DataNascita;

typedef struct {
    int matricola;
    DatiAnagrafici anagrafici;
    DatiLavorativi lavorativi;
} Dipendente;

// Accesso con dot notation o arrow
dip.anagrafici.cognome
dip->anagrafici.cognome  // Se dip è puntatore
```

#### Record in Ada
```ada
type Data_Nascita_Type is record
   Giorno : Integer range 1..31;
   Mese   : Integer range 1..12;
   Anno   : Integer range 1900..2100;
end record;

type Dipendente_Type is record
   Matricola  : Integer;
   Anagrafici : Dati_Anagrafici_Type;
   Lavorativi : Dati_Lavorativi_Type;
end record;
```

### 3.9 Puntatori a Funzione

In C le funzioni sono first-class citizens tramite puntatori:

```c
// Funzioni che operano su due interi
int somma(int a, int b) { return a + b; }
int sottrazione(int a, int b) { return a - b; }

// Dichiarazione di puntatore a funzione
int (*ptrFunzione)(int, int);

// Assegnazione e uso
ptrFunzione = somma;
printf("Somma: %d\n", ptrFunzione(5, 3));  // Output: 8

ptrFunzione = sottrazione;
printf("Sottrazione: %d\n", ptrFunzione(5, 3));  // Output: 2

// Callback - passare funzione come parametro
int eseguiOperazione(int x, int y, int (*operazione)(int, int)) {
    return operazione(x, y);
}

// Uso
printf("5 + 3 = %d\n", eseguiOperazione(5, 3, somma));
```

### 3.10 Mapping di Storage per Matrici

Passaggio di matrici multidimensionali in C:

```c
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

// Uso
int matrice[2][3] = {{1,2,3}, {4,5,6}};
stampa1(matrice, 2);
stampa2(&matrice[0][0], 2, 3);
```

### 3.11 Compilazione Separata in C

Esempio di organizzazione modulare:

**config.h** - Header file:
```c
#ifndef CONFIG_H
#define CONFIG_H

#ifdef DEBUG_MODE
    #define MAX_SIZE 10
    #define LOG(msg) printf("DEBUG: %s\n", msg)
#else
    #define MAX_SIZE 100
    #define LOG(msg) /* niente */
#endif

extern int global_counter;  // Dichiarazione

static int header_static_var = 42;  // Ogni file ha la sua copia

extern void print_config(void);
extern int increment_counter(void);

#endif
```

**main.c** - Programma principale:
```c
#include <stdio.h>
#include "config.h"

int main(void) {
    printf("Valore iniziale global_counter: %d\n", global_counter);
    print_config();
    increment_counter();
    printf("Nuovo valore global_counter: %d\n", global_counter);
    return 0;
}
```

### 3.12 Slide di Riferimento
- [Nomi, Scope e Binding](https://elearning.unimib.it/pluginfile.php/2014802/mod_resource/content/1/Linguaggi%20di%20Programmazione%20-%20Nomi%2C%20Scope%20e%20Binding.html)
- [Type System](https://elearning.unimib.it/pluginfile.php/2014803/mod_resource/content/1/type_system_slides%20%281%29.html)
- [Tipi di Dato](https://elearning.unimib.it/pluginfile.php/2014804/mod_resource/content/1/Linguaggi%20di%20Programmazione%20-%20Tipi%20di%20Dato%20%281%29.html)
- [Sottoprogrammi](https://elearning.unimib.it/pluginfile.php/2014805/mod_resource/content/1/Liguaggi-di-Programmazione-Cap9-Sottoprogrammi.html)
- [Implementazione dei Sottoprogrammi](https://elearning.unimib.it/pluginfile.php/2040575/mod_resource/content/3/subprograms_presentation%20completa.html)

---

## CODICE DI RIFERIMENTO

### Sezione 1 - Introduzione
- `MIPSSim.py` - Simulazione semplificata operazioni MIPS
- `MIPSSimCompleto.py` - Simulazione completa con classe MemorySimulator

### Sezione 2 - Imperative
- `Esempi-Base/main.c` - Primo programma C, array, puntatori
- `Compilazione-Separata/` - Esempi di compilazione modulare
- `Header e File/` - Esempio completo con header, macro, extern

### Sezione 3 - Memoria
- `Variabili/esempio_C.c` - Esempio allocazione static/stack/heap in C
- `Variabili/EsempioJava.java` - Esempio allocazione in Java
- `Variabili/Esempio_Ada.ada` - Esempio allocazione in Ada
- `Scope/Static-Scope.pas` - Esempio scoping statico Pascal
- `Scope/Dinamico-Perl.pl` - Esempio scoping dinamico Perl
- `Tipi/Record-C.c` - Esempi di struct in C
- `Tipi/Record-Ada.ada` - Esempi di record in Ada
- `Sottoprogrammi/Puntatori-Funzione-C.c` - Callback e puntatori a funzione
- `Sottoprogrammi/Storage-Mapping.c` - Passaggio matrici a funzioni

---

## Riepilogo Concetti Chiave

### Sezione 1
- Ragioni per studiare i linguaggi di programmazione
- Criteri di valutazione: leggibilità, scrivibilità, affidabilità
- Influenze: architettura macchina, metodologie di progetto
- Categorie di linguaggi: imperativi, funzionali, logici, OOP, scripting
- Metodi di implementazione: compilazione, interpretazione, ibridi

### Sezione 2
- Storia ed evoluzione dei linguaggi di programmazione
- Caratteristiche del linguaggio C
- Compilazione separata e linking
- Direttive del preprocessore

### Sezione 3
- Attributi delle variabili (nome, indirizzo, valore, tipo, lifetime, scope)
- Binding: compile-time, load-time, runtime
- Storage: static, stack-dynamic, heap-dynamic
- Scope: statico (lessicale) vs dinamico
- Record di attivazione
- Passaggio parametri: by value, by reference, by name
- Sistemi di tipi: statico/dinamico, forte/debole, implicito/esplicito
- Type safety e equivalenza dei tipi

---

*Documento generato il 20 Febbraio 2025*
*Basato sul corso di Linguaggi di Programmazione - Università degli Studi di Milano-Bicocca*
