# AGENTS.md — Istruzioni Complete per l'Implementazione del Progetto SSSP

> **LEGGI TUTTO QUESTO FILE PRIMA DI SCRIVERE QUALSIASI RIGA DI CODICE.**
> Ogni sezione è critica. Il mancato rispetto di una qualsiasi regola
> comporta il fallimento del progetto.

---

## -1. COME INIZIARE

Sei un agente. La tua working directory è:

```
/home/dvh/Documents/prog/PROJECT/IMPL/
```

Questo file (`AGENTS.md`) contiene TUTTE le istruzioni necessarie
per implementare il progetto da zero. Leggilo per intero.

Quando sei pronto, il tuo lavoro consiste in:

1. **Leggere** questo file (lo stai già facendo).
2. **Creare** le directory `Prolog/` e `Lisp/` qui dentro.
3. **Implementare** `Prolog/sssp.pl` e `Lisp/sssp.lisp`.
4. **Testare** tutto come descritto nella Sezione 12.
5. **Creare** `package.sh` (Sezione 13), eseguirlo, generare
   lo .zip finale.
6. **Pulire** i file temporanei (test, script).

Non chiedere conferma. Non chiedere chiarimenti. Tutto ciò che
ti serve è in questo file e nei documenti di riferimento sotto.
Parti e implementa tutto fino alla consegna.

### File di riferimento nella stessa directory

Se hai bisogno di consultare le specifiche originali del
professore, i file sono qui nella tua working directory:

| File | Contenuto |
|------|-----------|
| `SPEC.md` | **Specifica master** del progetto — contiene TUTTE le API richieste, gli esempi, le spiegazioni dettagliate, le note a piè di pagina. È la traduzione fedele del PDF del professore. **Consultalo** se hai dubbi su un predicato/funzione specifica, sulla semantica esatta, o sugli esempi d'uso forniti dal prof. |
| `README.md` | Introduzione al progetto dal forum del corso — contiene info su gruppi e scadenza. |
| `CONSEGNA.md` | Regole di consegna — struttura .zip, `Gruppo.txt`, formato nomi file. **Consultalo** per verificare la struttura di consegna. |
| `DOMANDE.md` | **Domande e risposte ufficiali** dal forum — chiarimenti del professore. Contiene decisioni importanti: pesi float OK, default peso = 1, `new_vertex`/`new_arc` creano implicitamente, no `->` e `;` in Prolog ma `is` va bene. **Consultalo** in caso di dubbio su comportamenti ambigui. |
| `RULES.md` | Regole di formattazione — 80 colonne, indentazione, spazi operatori. **Già integrate** in questo file (Sezione 1) ma puoi consultare l'originale. |

> **PRIORITÀ DI CONSULTAZIONE:** Questo file (AGENTS.md) è il
> tuo riferimento principale — riassume e integra tutto.
> In caso di dubbio specifico su un'API, vai a `SPEC.md`.
> In caso di dubbio su un comportamento, vai a `DOMANDE.md`.

---

## 0. IDENTITÀ E METADATI

- **Studente:** DANIEL VINCENT HORGAN
- **Matricola:** 928744
- **Corso:** Linguaggi di Programmazione — A.A. 2025-2026
- **Progetto:** E2P — Single Source Shortest Paths (SSSP)
- **Scadenza:** 28 febbraio 2026, ore 23:55 GMT+1

Ogni file sorgente (`sssp.pl`, `sssp.lisp`) **DEVE** iniziare con un
commento contenente nome e matricola:

**Prolog:**
```prolog
% Daniel Vincent Horgan - 928744
```

**Common Lisp:**
```lisp
;;; Daniel Vincent Horgan - 928744
```

---

## 1. REGOLE DI FORMATTAZIONE — INVIOLABILI

> **Il professore CANCELLA il codice che non rispetta queste regole.**
> Queste NON sono linee guida. Sono requisiti assoluti.

### 1.1 Limite di 80 colonne

- **NESSUNA** riga di codice, commento, o stringa deve MAI superare
  80 caratteri.
- Spezza le righe lunghe in modo leggibile. Usa indentazione per
  le continuazioni.
- Controlla OGNI riga prima di finalizzare.

### 1.2 Indentazione corretta

- **Prolog:** indenta con 4 spazi. Il corpo delle clausole deve
  essere indentato rispetto alla testa.
- **Common Lisp:** indenta con 2 spazi. Segui le convenzioni
  standard Lisp (allinea argomenti sotto il primo argomento, oppure
  indenta di 2 rispetto alla forma che li contiene).
- **MAI** usare tabulazioni. Solo spazi.

### 1.3 Spazi attorno agli operatori

- Tutti gli operatori (`is`, `=`, `<`, `>`, `=<`, `>=`, `=:=`,
  `\=`, `+`, `-`, `*`, `/`, ecc.) devono avere uno spazio prima e
  uno spazio dopo.
- **Eccezione:** le virgole hanno solo uno spazio DOPO, mai prima.
- Esempi corretti:
  ```prolog
  X is Y + 1
  Weight >= 0
  foo(A, B, C)
  ```
- Esempi SBAGLIATI (codice cassato):
  ```prolog
  X is Y+1
  Weight>=0
  foo(A,B,C)
  ```

### 1.4 Stile del codice — NON deve sembrare generato da IA

- Il codice deve sembrare scritto da uno studente di informatica
  bravo e motivato di 22 anni.
- Evita pattern ripetitivi meccanici. Varia leggermente lo stile
  dei commenti.
- Non commentare l'ovvio. Commenta solo dove serve chiarezza.
- Usa una quantità ragionevole di commenti — né troppi né troppo
  pochi.
- **TUTTI i commenti devono essere in ITALIANO.**
- Evita commenti del tipo "Questa funzione fa X" come un manuale.
  Usa commenti naturali tipo "Caso base: heap vuoto" o
  "Scambio padre-figlio per ripristinare la proprietà".
- Ogni tanto usa forme colloquiali nei commenti, come farebbe uno
  studente (es. "NB:", "Attenzione qui", "Il classico heapify").
- NON usare docstring lunghe e formali in Lisp. Brevi commenti
  con `;` o `;;` bastano.

---

## 2. VINCOLI TECNICI ASSOLUTI

### 2.1 Prolog

- **NO** condizionali `->` (if-then).
- **NO** disgiunzioni `;` (or).
- **NO** moduli (`:- module(...)`).
- Usa `assert`, `retract`, `retractall` per la base di dati.
- Usa `findall/3` per raccogliere risultati.
- Usa `listing/1` nei predicati `list_*`.
- Usa i cut (`!`) dove appropriato per evitare soluzioni spurie
  (i predicati di manipolazione della base-dati lasciano
  alternative sugli stack).
- Il file **DEVE** funzionare in **SWI-Prolog**.
- Dichiara tutti i predicati dinamici con `:- dynamic`.

### 2.2 Common Lisp

- **NO** packages (`defpackage`, `in-package`).
- **NO** librerie esterne per heap.
- Usa `defparameter` per le hash-tables globali.
- Usa `make-hash-table :test #'equal` per tutte le hash-tables.
- Il MINHEAP deve essere implementato con un **array
  monodimensionale** (la rappresentazione classica).
- Puoi usare `:adjustable t` e `adjust-array` per l'array
  dello heap.
- Il file **DEVE** funzionare in **LispWorks**.
- NON usare funzioni specifiche di SBCL o altre implementazioni
  che non esistano in LispWorks.

### 2.3 Entrambi i linguaggi

- **NON** usare librerie heap preesistenti. L'implementazione
  del MINHEAP è parte fondamentale del progetto.
- **NON** fare copia-incolla dal PDF/documento della specifica.
  Possono esserci caratteri UNICODE nascosti che rompono gli
  script di valutazione automatica.
- I pesi degli archi possono essere float (anche in Lisp).
- Se il peso di un arco non è specificato, il default è **1**.
- `new_vertex` e `new_arc` creano implicitamente il
  grafo/vertice se non esiste.

---

## 3. STRUTTURA DEI FILE DA PRODURRE

```
Horgan_Daniel_Vincent_928744_SSSP_LP_202602/
  Gruppo.txt
  Lisp/
    sssp.lisp
    README.txt
  Prolog/
    sssp.pl
    README.txt
```

### 3.1 File da creare

| File | Descrizione |
|------|-------------|
| `Prolog/sssp.pl` | Tutto il codice Prolog in un unico file |
| `Prolog/README.txt` | Istruzioni di caricamento e uso |
| `Lisp/sssp.lisp` | Tutto il codice Common Lisp in un unico file |
| `Lisp/README.txt` | Istruzioni di caricamento e uso |
| `Gruppo.txt` | Una riga: `928744, Horgan Daniel Vincent` |

### 3.2 Contenuto di Gruppo.txt

```
928744, Horgan Daniel Vincent
```

### 3.3 Contenuto dei README.txt

Brevi istruzioni su come caricare e testare il file. Per Prolog:
come fare `?- [sssp].` o `?- consult('sssp.pl').` e un esempio
d'uso. Per Lisp: come fare `(load "sssp.lisp")` e un esempio d'uso.
Tieni i README semplici e pratici. In italiano.

---

## 4. ARCHITETTURA DEL CODICE

Sia in Prolog che in Lisp, il codice è diviso in tre sezioni
logiche da implementare **in quest'ordine**:

1. **MINHEAP** — Coda a priorità (implementa e testa PRIMA)
2. **GRAFI** — Struttura dati e API per grafi diretti
3. **SSSP** — Algoritmo di Dijkstra e shortest path

> **IMPORTANTE:** Implementa e verifica il MINHEAP prima di
> procedere con Dijkstra. L'algoritmo SSSP dipende interamente
> dal corretto funzionamento dello heap.

---

## 5. SPECIFICA PROLOG — `sssp.pl`

### 5.1 Dichiarazioni dinamiche (inizio file)

```prolog
:- dynamic graph/1.
:- dynamic vertex/2.
:- dynamic arc/4.
:- dynamic heap/2.
:- dynamic heap_entry/4.
:- dynamic distance/3.
:- dynamic visited/2.
:- dynamic previous/3.
```

### 5.2 API GRAFI — Predicati richiesti

Ogni predicato opera sulla base-dati Prolog con assert/retract.

#### `new_graph(G)`
- Se `graph(G)` esiste già, ha successo (non duplica).
- Altrimenti, asserisce `graph(G)`.
- Usa il cut per evitare soluzioni multiple.

#### `delete_graph(G)`
- Rimuove `graph(G)` dalla base-dati.
- Rimuove TUTTI i `vertex(G, _)` associati.
- Rimuove TUTTI gli `arc(G, _, _, _)` associati.
- Usa `retractall` per le rimozioni in blocco.

#### `new_vertex(G, V)`
- Aggiunge `vertex(G, V)` alla base-dati.
- Se il vertice esiste già nel grafo, ha successo senza duplicare.
- Se il grafo G non esiste, crealo implicitamente
  (chiama `new_graph(G)`).

#### `vertices(G, Vs)`
- `Vs` è la lista di tutti i vertici di G.
- Usa `findall/3`.

#### `list_vertices(G)`
- Stampa i vertici usando `listing(vertex/2)` o un equivalente
  filtrato per G.

#### `new_arc(G, U, V, Weight)`
- Aggiunge `arc(G, U, V, Weight)` alla base-dati.
- Il peso deve essere >= 0.
- Se esiste già un arco `arc(G, U, V, _)`, **aggiornalo**
  (ritratta il vecchio, asserisci il nuovo).
- Crea i vertici U e V implicitamente se non esistono
  (chiama `new_vertex`).
- Se il grafo non esiste, crealo implicitamente.

#### `new_arc(G, U, V)`
- Scorciatoia: chiama `new_arc(G, U, V, 1)`.

#### `arcs(G, Es)`
- `Es` è la lista di tutti gli archi `arc(G, _, _, _)` di G.
- Usa `findall/3`.

#### `neighbors(G, V, Ns)`
- V deve essere un vertice di G.
- `Ns` è la lista degli archi `arc(G, V, N, W)` uscenti da V.
- Usa `findall/3`.

#### `list_arcs(G)`
- Stampa gli archi usando `listing/1`.

#### `list_graph(G)`
- Stampa vertici e archi del grafo G.

### 5.3 API MINHEAP — Predicati richiesti

Lo heap è rappresentato nella base-dati come:
- `heap(H, S)` — heap H con dimensione S.
- `heap_entry(H, P, K, V)` — entry in posizione P con
  chiave K e valore V.

La "posizione" P è un intero >= 1 (indicizzazione a 1).
L'albero binario implicito usa:
- Padre di P: `P // 2` (divisione intera)
- Figlio sinistro: `P * 2`
- Figlio destro: `P * 2 + 1`

#### `new_heap(H)`
- Se `heap(H, _)` esiste già, ha successo.
- Altrimenti, asserisce `heap(H, 0)`.

#### `delete_heap(H)`
- Rimuove `heap(H, _)` e tutte le `heap_entry(H, _, _, _)`.

#### `heap_size(H, S)`
- Vero quando S è la dimensione corrente dello heap H.
- Semplice: `heap(H, S)`.

#### `empty(H)`
- Vero quando lo heap H ha dimensione 0.

#### `not_empty(H)`
- Vero quando lo heap H ha dimensione > 0.

#### `head(H, K, V)`
- Vero quando l'entry in posizione 1 ha chiave K e valore V.
- Fallisce se lo heap è vuoto.

#### `insert(H, K, V)`
- Inserisce una nuova entry con chiave K e valore V.
- Incrementa la dimensione dello heap.
- Posiziona la nuova entry in fondo (posizione Size + 1).
- Esegue **heapify-up** (bubble-up): confronta con il padre e
  scambia se la chiave del figlio è minore, fino alla radice.
- Aggiorna `heap(H, S)` con la nuova dimensione.

#### `extract(H, K, V)`
- Ritorna K, V dell'entry con chiave minima (posizione 1).
- Sposta l'ultima entry in posizione 1.
- Decrementa la dimensione.
- Esegue **heapify-down** (sift-down): confronta con i figli e
  scambia con il figlio con chiave minore, fino alle foglie.
- Aggiorna `heap(H, S)` con la nuova dimensione.
- Se lo heap ha un solo elemento, semplicemente rimuovilo.

#### `modify_key(H, NewKey, OldKey, V)`
- Trova l'entry con chiave OldKey e valore V.
- Sostituisce la chiave con NewKey.
- Esegue heapify-up O heapify-down a seconda che NewKey sia
  minore o maggiore di OldKey.
- In Prolog questa operazione è relativamente efficiente perché
  possiamo cercare `heap_entry(H, P, OldKey, V)` direttamente.

#### `list_heap(H)`
- Usa `listing(heap_entry/4)` per stampare lo stato dello heap.

#### Note implementative per il MINHEAP Prolog

- Per **scambiare** due entry: ritratta entrambe, asserisci
  entrambe con le posizioni invertite.
- Per **aggiornare** la dimensione: ritratta `heap(H, OldS)`,
  asserisci `heap(H, NewS)`.
- Heapify-up: predicato ricorsivo che risale l'albero.
  Caso base: posizione 1 (radice) oppure chiave >= chiave padre.
- Heapify-down: predicato ricorsivo che scende l'albero.
  Caso base: nessun figlio (posizione > Size/2 circa) oppure
  chiave <= chiave di entrambi i figli.
  Attenzione: un nodo potrebbe avere solo il figlio sinistro.
- Quando confronti chiavi, usa `=<` e `<` (non `<=`).

### 5.4 API SSSP — Predicati richiesti

I predicati dinamici usati dall'algoritmo:
- `distance(G, V, D)` — distanza minima corrente dalla sorgente
- `visited(G, V)` — vertice già processato
- `previous(G, V, U)` — predecessore nel cammino minimo

#### `change_distance(G, V, NewDist)`
- Ha sempre successo.
- Ritratta tutte le `distance(G, V, _)`.
- Asserisce `distance(G, V, NewDist)`.

#### `change_previous(G, V, U)`
- Ha successo.
- Ritratta tutte le `previous(G, V, _)`.
- Asserisce `previous(G, V, U)`.

#### `dijkstra_sssp(G, Source)`
- Implementa l'algoritmo di Dijkstra completo.
- **Algoritmo:**
  1. Pulisci stato precedente: ritratta tutti i `distance/3`,
     `visited/2`, `previous/3` relativi a G.
  2. Inizializza le distanze: per ogni vertice V di G, se
     V = Source allora `distance(G, V, 0)`, altrimenti
     `distance(G, V, inf)`.
  3. Crea un nuovo heap temporaneo, inserisci (0, Source).
     Inserisci anche tutti gli altri vertici con chiave `inf`.
     Oppure, più semplicemente, inserisci solo la sorgente e
     aggiungi i vicini man mano (approccio lazy). **ENTRAMBI
     gli approcci sono validi**, ma l'approccio dove si inseriscono
     tutti i vertici all'inizio è più fedele al CLRS.
  4. Loop principale: finché lo heap non è vuoto:
     a. Estrai il minimo (D, V) dallo heap.
     b. Se V è già visitato, salta (continua).
     c. Segna V come visitato: asserisci `visited(G, V)`.
     d. Per ogni arco `arc(G, V, N, W)` dove N NON è visitato:
        - Calcola `NewDist = D + W`.
        - Se `NewDist < distance(G, N, OldDist)`:
          - `change_distance(G, N, NewDist)`
          - `change_previous(G, N, V)`
          - Aggiorna lo heap: usa `modify_key` se N è nello heap,
            oppure `insert` se non c'è.
  5. Cancella lo heap temporaneo.
- Usa `inf` come valore infinito. In SWI-Prolog `inf` è un
  atomo speciale che è maggiore di qualsiasi numero con `<` e `>`.
  Verifica che i confronti funzionino: `42 < inf` deve essere true.

#### `shortest_path(G, Source, V, Path)`
- Ricostruisce il cammino minimo da Source a V usando i fatti
  `previous/3` lasciati da `dijkstra_sssp`.
- `Path` è una lista di archi:
  `[arc(G, Source, N1, W1), arc(G, N1, N2, W2), ...,
   arc(G, NK, V, Wk)]`
- **Algoritmo:** Parti da V, risali con `previous(G, V, U)` fino
  a Source, accumula gli archi, poi inverti la lista.
- Se Source = V, il path è `[]` (lista vuota).
- Se V non è raggiungibile (nessun previous e V ≠ Source), il
  predicato fallisce.

---

## 6. SPECIFICA COMMON LISP — `sssp.lisp`

### 6.1 Hash-tables globali (inizio file)

```lisp
(defparameter *vertices* (make-hash-table :test #'equal))
(defparameter *arcs* (make-hash-table :test #'equal))
(defparameter *graphs* (make-hash-table :test #'equal))
(defparameter *visited* (make-hash-table :test #'equal))
(defparameter *distances* (make-hash-table :test #'equal))
(defparameter *previous* (make-hash-table :test #'equal))
(defparameter *heaps* (make-hash-table :test #'equal))
```

**NB:** `defparameter` re-inizializza le hash-tables ogni volta
che il file viene ricaricato. Questo è l'effetto voluto.

### 6.2 API GRAFI — Funzioni richieste

Le chiavi nelle hash-tables usano liste come chiavi composite.
Es: un vertice è indicizzato come `(vertex graph-id vertex-id)`.

#### `is-graph (graph-id)` → graph-id o NIL
- Ritorna il graph-id se il grafo esiste in `*graphs*`, NIL
  altrimenti.

#### `new-graph (graph-id)` → graph-id
- Se il grafo esiste, ritorna graph-id.
- Altrimenti, inserisce graph-id in `*graphs*` con
  `(setf (gethash graph-id *graphs*) graph-id)`.

#### `delete-graph (graph-id)` → NIL
- Rimuove il grafo da `*graphs*`.
- Rimuove TUTTI i vertici associati da `*vertices*`.
- Rimuove TUTTI gli archi associati da `*arcs*`.
- Rimuove eventuali dati SSSP da `*visited*`, `*distances*`,
  `*previous*`.
- Usa `maphash` + `remhash` per filtrare le entry per graph-id.

#### `new-vertex (graph-id vertex-id)` → vertex-rep
- La rappresentazione di un vertice è:
  `(vertex graph-id vertex-id)`
- Chiave in `*vertices*`: `(vertex graph-id vertex-id)`
- Valore: `(vertex graph-id vertex-id)`
- Se il grafo non esiste, crealo implicitamente.

#### `graph-vertices (graph-id)` → lista di vertex-rep
- Ritorna una lista di tutti i vertici del grafo.
- Itera su `*vertices*` con `maphash`, filtra per graph-id.

#### `new-arc (graph-id u v &optional (weight 1))` → arc-rep
- La rappresentazione di un arco è: `(arc graph-id u v weight)`
- Chiave in `*arcs*`: `(arc graph-id u v)`
- Valore: `(arc graph-id u v weight)`
- Il peso deve essere >= 0.
- Se esiste già un arco (G, U, V), **aggiornalo** col nuovo peso.
- Crea i vertici e il grafo implicitamente se non esistono.

#### `graph-arcs (graph-id)` → lista di arc-rep
- Ritorna tutti gli archi del grafo.
- Itera su `*arcs*` con `maphash`, filtra per graph-id.

#### `graph-vertex-neighbors (graph-id vertex-id)` → lista di arc-rep
- Ritorna gli archi uscenti da vertex-id nel grafo.
- Filtra `*arcs*` per graph-id e vertice sorgente = vertex-id.

#### `graph-print (graph-id)`
- Stampa vertici e archi del grafo sulla console.

### 6.3 API MINHEAP — Funzioni richieste

Lo heap è rappresentato come una lista nella hash-table `*heaps*`:

```lisp
(heap heap-id heap-size actual-heap-array)
```

L'array è monodimensionale, indicizzato a 0. Ogni cella contiene
una entry `(K V)` — una lista di due elementi.

L'albero binario implicito (indicizzazione a 0):
- Padre di i: `(floor (- i 1) 2)`
- Figlio sinistro di i: `(+ (* 2 i) 1)`
- Figlio destro di i: `(+ (* 2 i) 2)`

**OPPURE** puoi usare indicizzazione a 1 (ignori posizione 0):
- Padre di i: `(floor i 2)`
- Figlio sinistro: `(* 2 i)`
- Figlio destro: `(+ (* 2 i) 1)`

Scegli UN approccio e sii coerente. L'indicizzazione a 1 è
più semplice e fedele al CLRS. In tal caso heap-size indica
quanti elementi ci sono e gli elementi vanno da indice 1 a
heap-size nell'array. La posizione 0 dell'array resta inutilizzata.

Funzioni di accesso alla heap-rep:

```lisp
(defun heap-id (heap-rep) (second heap-rep))
(defun heap-size (heap-rep) (third heap-rep))
(defun heap-actual-heap (heap-rep) (fourth heap-rep))
```

**Attenzione:** `heap-size` come nome di funzione potrebbe
confliggere con la funzione di accesso. Puoi usare un approccio
diverso, ma l'interfaccia pubblica deve corrispondere.

#### `new-heap (heap-id &optional (capacity 42))` → heap-rep
- Se esiste, ritorna lo heap esistente.
- Altrimenti, crea con `make-array` (suggerito: usa
  `:adjustable t` e `:initial-element nil`).
- Inserisci in `*heaps*`.

#### `heap-delete (heap-id)` → T
- Rimuove lo heap da `*heaps*` con `remhash`.

#### `heap-empty (heap-id)` → boolean
- Vero se heap-size = 0.

#### `heap-not-empty (heap-id)` → boolean
- Vero se heap-size > 0.

#### `heap-head (heap-id)` → (K V)
- Ritorna la coppia (chiave, valore) in cima allo heap (la
  entry con chiave minima).
- Non rimuove l'elemento.
- Se lo heap è vuoto, errore o NIL.

#### `heap-insert (heap-id K V)` → boolean
- Inserisce entry con chiave K e valore V.
- Se l'array è pieno, allargalo (usa `adjust-array` se
  adjustable, oppure crea un nuovo array più grande e copia).
- Posiziona in fondo, fai heapify-up.
- Aggiorna heap-size nella heap-rep.
- **Ricorda:** la heap-rep è una lista nella hash-table, quindi
  per aggiornare heap-size devi modificare la lista con `setf`
  (es. `(setf (third heap-rep) new-size)`) OPPURE rimpiazzare
  l'intera heap-rep nella hash-table.

#### `heap-extract (heap-id)` → (K V)
- Estrae e ritorna l'elemento con chiave minima.
- Sposta l'ultimo elemento in cima.
- Decrementa heap-size.
- Fai heapify-down.

#### `heap-modify-key (heap-id new-key old-key V)` → boolean
- Trova l'entry con chiave old-key e valore V nell'array.
- Sostituisci la chiave con new-key.
- Fai heapify-up o heapify-down a seconda.
- **NOTA della specifica:** in Lisp questa operazione è lineare
  (O(n)) perché bisogna cercare nell'array. Questo è accettabile.
  Se vuoi ottimizzare, puoi aggiungere una hash-table ausiliaria
  che mappa V alla sua posizione nell'array — ma non è richiesto.

#### `heap-print (heap-id)` → boolean
- Stampa lo stato interno dello heap.
- Formato libero, serve per debugging.

#### Note implementative per il MINHEAP Lisp

- Per scambiare due entry nell'array:
  ```lisp
  (rotatef (aref array i) (aref array j))
  ```
  Oppure usa una variabile temporanea.
- Per confrontare chiavi, usa `<`, `<=`.
- Heapify-up e heapify-down possono essere funzioni ricorsive
  o iterative (loop). In Lisp entrambi gli approcci sono OK.
  Un `do` o `loop` iterativo è perfettamente accettabile.
- Ricorda di gestire il caso "solo figlio sinistro" in
  heapify-down.

### 6.4 API SSSP — Funzioni richieste

Le hash-tables per SSSP usano come chiave `(graph-id vertex-id)`.

#### `sssp-dist (graph-id vertex-id)` → d
- Ritorna la distanza dalla sorgente a vertex-id.
- `(gethash (list graph-id vertex-id) *distances*)`

#### `sssp-visited (graph-id vertex-id)` → boolean
- Ritorna T se vertex-id è stato visitato.
- `(gethash (list graph-id vertex-id) *visited*)`

#### `sssp-previous (graph-id V)` → U
- Ritorna il predecessore di V nel cammino minimo.
- `(gethash (list graph-id V) *previous*)`

#### `sssp-change-dist (graph-id V new-dist)` → NIL
- `(setf (gethash (list graph-id V) *distances*) new-dist)`
- Ritorna NIL (non il valore settato).

#### `sssp-change-previous (graph-id V U)` → NIL
- `(setf (gethash (list graph-id V) *previous*) U)`
- Ritorna NIL.

#### `sssp-dijkstra (graph-id source)` → NIL
- Implementa l'algoritmo di Dijkstra.
- **Algoritmo:**
  1. Pulisci stato precedente per graph-id: rimuovi tutte le
     entry in `*distances*`, `*visited*`, `*previous*` relative
     a graph-id.
  2. Inizializza: per ogni vertice V, setta distanza a
     `most-positive-double-float` (infinito). Per Source,
     setta distanza a 0.
  3. Crea un heap temporaneo. Inserisci `(0, Source)`.
     Inserisci gli altri vertici con chiave
     `most-positive-double-float`.
  4. Loop: finché lo heap non è vuoto:
     a. Estrai minimo (D, V).
     b. Se V è visitato, salta.
     c. Marca V come visitato.
     d. Per ogni arco (G, V, N, W):
        - Se N non è visitato:
          - `new-dist = D + W`
          - Se `new-dist < (sssp-dist graph-id N)`:
            - `sssp-change-dist graph-id N new-dist`
            - `sssp-change-previous graph-id N V`
            - Aggiorna lo heap con `heap-modify-key`.
  5. Elimina lo heap temporaneo.
  6. Ritorna NIL.
- Per il valore infinito in Lisp, usa `most-positive-double-float`.

#### `sssp-shortest-path (graph-id source V)` → path
- Ricostruisce il cammino minimo come lista di arc-rep.
- Risali da V usando `sssp-previous` fino a Source.
- Per ogni coppia (U, Next) nel cammino, trova l'arco
  corrispondente da `*arcs*`.
- Ritorna la lista ordinata da Source a V:
  ```lisp
  ((arc G Source N1 W1) (arc G N1 N2 W2) ... (arc G NK V Wk))
  ```
- Se Source = V, ritorna NIL (lista vuota).

---

## 7. DETTAGLI DELL'ALGORITMO DI DIJKSTRA

Riferimento: CLRS [CLR+09] Sezione 24.3.

### Pseudocodice

```
DIJKSTRA(G, s):
  per ogni vertice v in G:
    dist[v] = infinito
    prev[v] = undefined
  dist[s] = 0
  Q = min-priority-queue con tutti i vertici, chiave = dist

  while Q non è vuoto:
    u = EXTRACT-MIN(Q)
    marca u come visitato
    per ogni vicino v di u (non visitato):
      alt = dist[u] + peso(u, v)
      se alt < dist[v]:
        dist[v] = alt
        prev[v] = u
        DECREASE-KEY(Q, v, alt)
```

### Note critiche

- L'operazione DECREASE-KEY corrisponde a `modify_key` /
  `heap-modify-key`.
- Il grafo è **diretto**: gli archi hanno una direzione.
  `arc(G, U, V, W)` è diverso da `arc(G, V, U, W)`.
- I pesi sono **non-negativi** (>= 0).
- Vertici non raggiungibili dalla sorgente avranno distanza
  `inf` / `most-positive-double-float` e nessun predecessore.

---

## 8. STRUTTURA INTERNA DEI FILE

### 8.1 `sssp.pl` — Ordine delle sezioni

```
1. Commento iniziale (nome, matricola)
2. Dichiarazioni :- dynamic
3. ---- SEZIONE MINHEAP ----
   - new_heap/1, delete_heap/1
   - heap_size/2, empty/1, not_empty/1
   - head/3
   - insert/3 (con heapify-up)
   - extract/3 (con heapify-down)
   - modify_key/4
   - list_heap/1
   - Predicati ausiliari: swap, heapify_up, heapify_down
4. ---- SEZIONE GRAFI ----
   - new_graph/1, delete_graph/1
   - new_vertex/2, vertices/2, list_vertices/1
   - new_arc/3, new_arc/4, arcs/2
   - neighbors/3, list_arcs/1, list_graph/1
5. ---- SEZIONE SSSP ----
   - distance/3, visited/2, previous/3 (dinamici)
   - change_distance/3, change_previous/3
   - dijkstra_sssp/2
   - shortest_path/4
   - Predicati ausiliari per Dijkstra
```

### 8.2 `sssp.lisp` — Ordine delle sezioni

```
1. Commento iniziale (nome, matricola)
2. Definizioni hash-tables globali (defparameter)
3. ---- SEZIONE MINHEAP ----
   - new-heap, heap-delete
   - heap-empty, heap-not-empty
   - heap-head
   - heap-insert (con heapify-up)
   - heap-extract (con heapify-down)
   - heap-modify-key
   - heap-print
   - Funzioni ausiliarie: swap, heapify-up, heapify-down
4. ---- SEZIONE GRAFI ----
   - is-graph, new-graph, delete-graph
   - new-vertex, graph-vertices
   - new-arc, graph-arcs
   - graph-vertex-neighbors
   - graph-print
5. ---- SEZIONE SSSP ----
   - sssp-dist, sssp-visited, sssp-previous
   - sssp-change-dist, sssp-change-previous
   - sssp-dijkstra
   - sssp-shortest-path
```

---

## 9. TESTING

### 9.1 Grafo di test base (da CLRS)

Usa questo grafo per verificare la correttezza:

```
Vertici: s, t, x, y, z
Archi:
  s -> t, peso 10
  s -> y, peso 5
  t -> x, peso 1
  t -> y, peso 2
  x -> z, peso 4
  y -> t, peso 3
  y -> x, peso 9
  y -> z, peso 2
  z -> s, peso 7
  z -> x, peso 6
```

**Risultati attesi da sorgente s:**
| Vertice | Distanza | Predecessore |
|---------|----------|--------------|
| s       | 0        | -            |
| t       | 8        | y            |
| x       | 9        | t            |
| y       | 5        | s            |
| z       | 7        | y            |

**Shortest path s → x:** s→y (5), y→t (3), t→x (1) = 9

### 9.2 Test del MINHEAP (esegui PRIMA)

**Prolog:**
```prolog
?- new_heap(test_heap).
?- insert(test_heap, 5, a).
?- insert(test_heap, 3, b).
?- insert(test_heap, 7, c).
?- insert(test_heap, 1, d).
?- head(test_heap, K, V).
% Atteso: K = 1, V = d
?- extract(test_heap, K, V).
% Atteso: K = 1, V = d
?- head(test_heap, K, V).
% Atteso: K = 3, V = b
?- modify_key(test_heap, 2, 7, c).
?- head(test_heap, K, V).
% Atteso: K = 2, V = c
?- delete_heap(test_heap).
```

**Common Lisp:**
```lisp
(new-heap 'test-heap)
(heap-insert 'test-heap 5 'a)
(heap-insert 'test-heap 3 'b)
(heap-insert 'test-heap 7 'c)
(heap-insert 'test-heap 1 'd)
(heap-head 'test-heap)
;; Atteso: (1 D)
(heap-extract 'test-heap)
;; Atteso: (1 D)
(heap-head 'test-heap)
;; Atteso: (3 B)
(heap-modify-key 'test-heap 2 7 'c)
(heap-head 'test-heap)
;; Atteso: (2 C)
(heap-delete 'test-heap)
```

### 9.3 Test SSSP completo

**Prolog:**
```prolog
?- new_graph(g).
?- new_arc(g, s, t, 10).
?- new_arc(g, s, y, 5).
?- new_arc(g, t, x, 1).
?- new_arc(g, t, y, 2).
?- new_arc(g, x, z, 4).
?- new_arc(g, y, t, 3).
?- new_arc(g, y, x, 9).
?- new_arc(g, y, z, 2).
?- new_arc(g, z, s, 7).
?- new_arc(g, z, x, 6).
?- dijkstra_sssp(g, s).
?- shortest_path(g, s, x, Path).
% Atteso: Path = [arc(g, s, y, 5), arc(g, y, t, 3),
%                 arc(g, t, x, 1)]
?- distance(g, z, D).
% Atteso: D = 7
```

**Common Lisp:**
```lisp
(new-graph 'g)
(new-arc 'g 's 't 10)
(new-arc 'g 's 'y 5)
(new-arc 'g 't 'x 1)
(new-arc 'g 't 'y 2)
(new-arc 'g 'x 'z 4)
(new-arc 'g 'y 't 3)
(new-arc 'g 'y 'x 9)
(new-arc 'g 'y 'z 2)
(new-arc 'g 'z 's 7)
(new-arc 'g 'z 'x 6)
(sssp-dijkstra 'g 's)
(sssp-shortest-path 'g 's 'x)
;; Atteso: ((ARC G S Y 5) (ARC G Y T 3) (ARC G T X 1))
(sssp-dist 'g 'z)
;; Atteso: 7
```

### 9.4 Edge cases da testare

- Grafo con un solo vertice (sorgente).
- Arco con peso 0.
- Vertici non raggiungibili dalla sorgente.
- Grafo con archi di peso float (es. 4.2).
- `shortest_path` dove Source = V (path vuoto).
- Chiamata a `dijkstra_sssp` su un grafo dove è già stato
  chiamato (deve sovrascrivere i risultati precedenti).
- Heap con un solo elemento: insert poi extract.
- Heap: insert multipli con la stessa chiave.
- `new_arc` con vertice/grafo inesistente (creazione implicita).
- `new_arc` che aggiorna il peso di un arco esistente.

---

## 10. ERRORI COMUNI DA EVITARE

1. **Dimenticare il cut** nei predicati che usano assert/retract.
   Senza cut, Prolog può generare soluzioni spurie.

2. **Non gestire il caso "solo figlio sinistro"** in heapify-down.
   Se la dimensione è pari, l'ultimo nodo interno ha solo il
   figlio sinistro.

3. **Usare `<=` invece di `=<`** in Prolog. L'operatore "minore o
   uguale" in Prolog è `=<`, NON `<=`.

4. **Non pulire lo stato precedente** quando si richiama
   `dijkstra_sssp`. Bisogna ritrattare tutti i distance/visited/
   previous relativi al grafo prima di iniziare.

5. **Confondere `=` con `is`** in Prolog. Usa `is` per valutazioni
   aritmetiche, `=` per unificazione.

6. **Non gestire l'infinito correttamente**. In Prolog usa `inf`.
   In Lisp usa `most-positive-double-float`.

7. **Superare le 80 colonne.** Controlla OGNI riga. Il prof
   cancella il codice.

8. **Usare `->` o `;`** in Prolog. Usa clausole multiple con
   pattern matching e cut.

9. **Dimenticare gli spazi attorno agli operatori.** `X is Y + 1`,
   non `X is Y+1`.

10. **Commenti in inglese.** TUTTI i commenti devono essere in
    italiano.

11. **Non aggiornare un arco esistente.** Se `new_arc(g, a, b, 5)`
    viene chiamato e poi `new_arc(g, a, b, 3)`, il peso deve
    diventare 3 (non avere due archi).

12. **Non cancellare lo heap temporaneo** dopo `dijkstra_sssp`.

---

## 11. CHECKLIST FINALE

Prima di consegnare, verifica OGNI punto:

- [ ] Ogni file inizia con commento nome + matricola.
- [ ] NESSUNA riga supera 80 caratteri.
- [ ] Indentazione corretta e consistente.
- [ ] Spazi attorno a TUTTI gli operatori.
- [ ] Spazio dopo OGNI virgola.
- [ ] Tutti i commenti sono in italiano.
- [ ] Nessun uso di `->` o `;` in Prolog.
- [ ] Nessun modulo Prolog, nessun package Lisp.
- [ ] Tutti i predicati dinamici dichiarati con `:- dynamic`.
- [ ] MINHEAP testato indipendentemente.
- [ ] Grafi testati indipendentemente.
- [ ] Dijkstra testato con il grafo CLRS (risultati corretti).
- [ ] `shortest_path` produce il formato corretto.
- [ ] Edge cases testati.
- [ ] `sssp.pl` caricabile in SWI-Prolog senza errori.
- [ ] `sssp.lisp` caricabile in LispWorks senza errori.
- [ ] Struttura .zip corretta.
- [ ] `Gruppo.txt` presente con formato corretto.
- [ ] README.txt presenti in entrambe le sottodirectory.
- [ ] Il codice non sembra generato da IA.
- [ ] Nessun carattere UNICODE spurio nel codice.
- [ ] Il nome del .zip è esattamente:
      `Horgan_Daniel_Vincent_928744_SSSP_LP_202602.zip`

---

## 12. AMBIENTE DI SVILUPPO E TESTING

### 12.1 Strumenti disponibili

I seguenti strumenti sono **già installati** sulla macchina:

- **SWI-Prolog** — comando: `swipl`
- **SBCL (Steel Bank Common Lisp)** — comando: `sbcl`
- **Bash** — per scripting
- **zip** — per creare l'archivio di consegna

> **NOTA:** Il progetto deve funzionare in **LispWorks**, ma
> testiamo con SBCL che è disponibile. SBCL è molto compatibile
> con lo standard Common Lisp. Evita solo funzioni specifiche di
> SBCL (es. `sb-ext:*`). Se funziona in SBCL con puro Common
> Lisp standard, funzionerà in LispWorks.

### 12.2 Come eseguire i test

**Prolog — test interattivo:**
```bash
swipl -l Prolog/sssp.pl
```
Poi digita query al prompt `?-`.

**Prolog — test da script (non interattivo):**
```bash
swipl -l Prolog/sssp.pl -g "goal1, goal2, halt."
```
Oppure crea un file `.pl` di test e caricalo:
```bash
swipl -l Prolog/sssp.pl -l test_prolog.pl -g "run_tests, halt."
```

**Common Lisp — test interattivo:**
```bash
sbcl --load Lisp/sssp.lisp
```
Poi digita espressioni al prompt `*`.

**Common Lisp — test da script:**
```bash
sbcl --load Lisp/sssp.lisp --eval '(progn (test1) (test2))' \
     --quit
```

### 12.3 Strategia di testing — TESTA TUTTO A MORTE

**L'agente DEVE eseguire ogni singolo test sotto descritto.**
Non basta implementare e "sperare che funzioni". Il professore
usa script di valutazione automatica. Se qualcosa non torna,
è zero punti su quel predicato/funzione.

#### Fase 1: Test di caricamento

Verifica che i file si carichino senza errori e senza warning:

```bash
# Prolog: deve caricare senza errori
swipl -l Prolog/sssp.pl -g halt 2>&1
# Output atteso: nessun errore, nessun warning

# Lisp: deve caricare senza errori
sbcl --noinform --load Lisp/sssp.lisp --quit 2>&1
# Output atteso: nessun errore, nessun warning
```

Se ci sono warning (es. "redefined", "singleton variable"),
**correggili**. Variabili singleton in Prolog vanno rinominate
con prefisso `_` (es. `_Unused`).

#### Fase 2: Test MINHEAP isolato

Testa lo heap **PRIMA** di tutto il resto. Crea file di test
temporanei. Verifica TUTTI questi scenari:

**Test heap base:**
1. `new_heap` — crea heap, verificane l'esistenza.
2. `insert` — inserisci 1 elemento, verifica `head`.
3. `insert` multipli — inserisci 5+ elementi con chiavi casuali,
   verifica che `head` sia sempre il minimo.
4. `extract` — estrai, verifica che sia il minimo, verifica che
   `head` sia il nuovo minimo.
5. `extract` tutti — estrai tutti uno a uno, verifica che escano
   in ordine crescente di chiave.
6. `empty`/`not_empty` — verifica coerenza con heap_size.
7. `modify_key` — riduci una chiave, verifica che la proprietà
   heap sia mantenuta. Aumenta una chiave, stessa verifica.
8. `delete_heap` — dopo cancellazione, `heap_size` deve fallire.

**Test heap stress:**
9. Inserisci 100+ elementi, estraili tutti, verifica ordine.
10. Inserisci elementi con chiavi duplicate.
11. Inserisci, estrai alcuni, inserisci altri, estrai tutti.
12. `modify_key` su elemento che diventa il nuovo minimo.
13. `modify_key` su elemento che era il minimo (aumenta chiave).

**Test heap edge cases:**
14. Heap con un solo elemento: insert, extract.
15. Heap vuoto: `empty` deve essere true, `extract` deve fallire.
16. Chiavi float (es. 3.14, 2.71).
17. Valori complessi come chiavi (atomi, stringhe, liste).

#### Fase 3: Test GRAFI isolato

**Test grafi base:**
1. `new_graph` — crea grafo, verificane l'esistenza.
2. `new_graph` duplicato — non deve creare duplicati.
3. `new_vertex` — aggiungi vertici, verifica con `vertices`.
4. `new_arc` — aggiungi archi, verifica con `arcs`.
5. `new_arc` default peso — verifica che il peso sia 1.
6. `neighbors` — verifica che ritorni solo archi uscenti.
7. `delete_graph` — dopo cancellazione, vertici e archi devono
   sparire.

**Test grafi avanzati:**
8. `new_arc` con grafo/vertici inesistenti — deve crearli.
9. `new_arc` aggiornamento peso — sovrascrivi un arco esistente.
10. Vertice in più grafi distinti.
11. `vertices` e `arcs` su grafo vuoto (solo `new_graph`).
12. `list_vertices`, `list_arcs`, `list_graph` — devono stampare
    senza errori (non serve verificare il formato esatto).

#### Fase 4: Test SSSP / Dijkstra

**Test con grafo CLRS (grafo principale):**
```
s->t:10, s->y:5, t->x:1, t->y:2, x->z:4,
y->t:3, y->x:9, y->z:2, z->s:7, z->x:6
```
Risultati attesi da sorgente s:
- distance(g, s, 0)
- distance(g, y, 5)
- distance(g, z, 7)
- distance(g, t, 8)
- distance(g, x, 9)
- previous(g, y, s)
- previous(g, z, y)
- previous(g, t, y)
- previous(g, x, t)
- shortest_path(g, s, z, [arc(g,s,y,5), arc(g,y,z,2)])
- shortest_path(g, s, x,
    [arc(g,s,y,5), arc(g,y,t,3), arc(g,t,x,1)])

**Test con grafo semplice (triangolo):**
```
a->b:1, b->c:2, a->c:10
```
Da sorgente a:
- distance = {a:0, b:1, c:3}
- shortest_path(a, c) = [a->b:1, b->c:2] (non a->c:10)

**Test con archi di peso 0:**
```
a->b:0, b->c:0
```
Da sorgente a: distance = {a:0, b:0, c:0}

**Test con grafo disconnesso:**
```
a->b:1 (nessun arco verso c)
```
Da sorgente a: distance(c) = inf, shortest_path(a,c) fallisce.

**Test con un solo vertice:**
```
new_vertex(g, solo)
```
Da sorgente solo: distance = 0, shortest_path(solo, solo) = []

**Test con pesi float:**
```
a->b:1.5, b->c:2.3, a->c:5.0
```
Da sorgente a: distance(c) = 3.8

**Test riesecuzione Dijkstra:**
Esegui dijkstra_sssp su un grafo, poi rieseguilo con sorgente
diversa. I risultati vecchi devono essere sovrascritti.

**Test grafo più grande (10+ vertici):**
Crea un grafo con almeno 10 vertici e 20+ archi. Verifica
manualmente (o con calcolo) alcuni shortest path.

#### Fase 5: Test di formattazione

Dopo che tutto funziona, verifica la formattazione:

```bash
# Verifica 80 colonne — deve stampare ZERO righe
awk 'length > 80' Prolog/sssp.pl
awk 'length > 80' Lisp/sssp.lisp

# Verifica nessun TAB
grep -P '\t' Prolog/sssp.pl && echo "ERRORE: TAB trovato!"
grep -P '\t' Lisp/sssp.lisp && echo "ERRORE: TAB trovato!"

# Verifica niente -> o ; in Prolog (esclusi commenti e stringhe)
# Questo è un check approssimativo, verifica manualmente
grep -n '\->' Prolog/sssp.pl
grep -n '[^a-zA-Z];[^)]' Prolog/sssp.pl

# Verifica nessun commento in inglese (check manuale)
# Scorri tutti i commenti e assicurati siano in italiano

# Verifica niente moduli Prolog
grep -n ':- module' Prolog/sssp.pl

# Verifica niente package Lisp
grep -n 'defpackage\|in-package' Lisp/sssp.lisp
```

#### Fase 6: Test di integrazione completo

Esegui un test end-to-end in una singola sessione pulita
per ENTRAMBI i linguaggi:

**Prolog end-to-end:**
```prolog
% Pulisci tutto
% Crea grafo, aggiungi vertici e archi
% Esegui dijkstra_sssp
% Verifica TUTTE le distanze
% Verifica TUTTI i predecessori
% Verifica TUTTI gli shortest_path
% Cancella il grafo
% Verifica che sia effettivamente cancellato
```

**Lisp end-to-end:**
```lisp
;; Stessa sequenza di sopra ma in Lisp
;; Ricorda: dopo (load "sssp.lisp") le hash-tables sono pulite
```

### 12.4 Script di test automatizzati

Crea questi file di test temporanei, eseguili, poi eliminali.
I test devono **stampare chiaramente PASS o FAIL** per ogni
singolo test case. Se anche UN SOLO test fallisce, non procedere
alla consegna — correggi prima il bug.

**Schema per test Prolog (file temporaneo `test_all.pl`):**
```prolog
:- use_module(library(format)).

test_pass(Name) :-
    format("PASS: ~w~n", [Name]).
test_fail(Name) :-
    format("FAIL: ~w~n", [Name]).

run_test(Name, Goal) :-
    (call(Goal) -> test_pass(Name) ; test_fail(Name)).

run_all_tests :-
    % ... tutti i test qui ...
    halt.
```
**Nota:** usa `->` SOLO nel file di test, MAI in sssp.pl.

**Schema per test Lisp (file temporaneo `test_all.lisp`):**
```lisp
(defun run-test (name test-fn)
  (if (funcall test-fn)
      (format t "PASS: ~a~%" name)
      (format t "FAIL: ~a~%" name)))

;; ... tutti i test qui ...
```

Dopo aver eseguito tutti i test e verificato che passano,
**elimina i file di test temporanei** — non devono finire
nella consegna.

---

## 13. SCRIPT DI CONSEGNA (HAND-IN)

Dopo che TUTTI i test passano, genera la struttura di consegna
esatta creando ed eseguendo questo script bash.

### 13.1 Script `package.sh`

Crea il file `package.sh` nella directory del progetto con
questo contenuto esatto, poi eseguilo con `bash package.sh`.

```bash
#!/usr/bin/env bash
set -euo pipefail

# --------------------------------------------------------
# Script di packaging per la consegna del progetto SSSP
# Daniel Vincent Horgan - 928744
# --------------------------------------------------------

NOME="Horgan_Daniel_Vincent_928744_SSSP_LP_202602"
DIR_BASE="$(cd "$(dirname "$0")" && pwd)"
BUILD_DIR="${DIR_BASE}/${NOME}"
ZIP_FILE="${DIR_BASE}/${NOME}.zip"

# Pulisci build precedenti
rm -rf "${BUILD_DIR}" "${ZIP_FILE}"

# Crea struttura directory
mkdir -p "${BUILD_DIR}/Prolog"
mkdir -p "${BUILD_DIR}/Lisp"

# Copia sorgenti
cp "${DIR_BASE}/Prolog/sssp.pl"   "${BUILD_DIR}/Prolog/sssp.pl"
cp "${DIR_BASE}/Lisp/sssp.lisp"   "${BUILD_DIR}/Lisp/sssp.lisp"

# Crea Gruppo.txt
cat > "${BUILD_DIR}/Gruppo.txt" << 'EOF'
928744, Horgan Daniel Vincent
EOF

# Crea README Prolog
cat > "${BUILD_DIR}/Prolog/README.txt" << 'EOF'
Progetto SSSP - Linguaggi di Programmazione 2025-2026
Daniel Vincent Horgan - 928744

Caricamento in SWI-Prolog:
  ?- consult('sssp.pl').
  oppure
  ?- [sssp].

Esempio d'uso:
  ?- new_graph(g).
  ?- new_arc(g, a, b, 3).
  ?- new_arc(g, b, c, 2).
  ?- new_arc(g, a, c, 10).
  ?- dijkstra_sssp(g, a).
  ?- shortest_path(g, a, c, Path).
  Path = [arc(g, a, b, 3), arc(g, b, c, 2)]

Il file sssp.pl contiene tre librerie:
  1. MINHEAP - coda a priorita'
  2. GRAFI - struttura dati per grafi diretti
  3. SSSP - algoritmo di Dijkstra
EOF

# Crea README Lisp
cat > "${BUILD_DIR}/Lisp/README.txt" << 'EOF'
Progetto SSSP - Linguaggi di Programmazione 2025-2026
Daniel Vincent Horgan - 928744

Caricamento in LispWorks (o SBCL):
  (load "sssp.lisp")

Esempio d'uso:
  (new-graph 'g)
  (new-arc 'g 'a 'b 3)
  (new-arc 'g 'b 'c 2)
  (new-arc 'g 'a 'c 10)
  (sssp-dijkstra 'g 'a)
  (sssp-shortest-path 'g 'a 'c)
  ;; => ((ARC G A B 3) (ARC G B C 2))

Il file sssp.lisp contiene tre librerie:
  1. MINHEAP - coda a priorita'
  2. GRAFI - struttura dati per grafi diretti
  3. SSSP - algoritmo di Dijkstra
EOF

# --------------------------------------------------------
# Validazione pre-packaging
# --------------------------------------------------------
echo "=== Validazione pre-packaging ==="

ERRORS=0

# Verifica che i sorgenti esistano
for f in "${BUILD_DIR}/Prolog/sssp.pl" \
         "${BUILD_DIR}/Lisp/sssp.lisp"; do
    if [ ! -f "$f" ]; then
        echo "ERRORE: $f non trovato!"
        ERRORS=$((ERRORS + 1))
    fi
done

# Verifica 80 colonne
for f in "${BUILD_DIR}/Prolog/sssp.pl" \
         "${BUILD_DIR}/Lisp/sssp.lisp"; do
    LONG=$(awk 'length > 80' "$f" | wc -l)
    if [ "$LONG" -gt 0 ]; then
        echo "ERRORE: $f ha $LONG righe > 80 colonne:"
        awk 'length > 80 {printf "  L%d (%d car): %s\n",
             NR, length, $0}' "$f"
        ERRORS=$((ERRORS + 1))
    fi
done

# Verifica nessun TAB
for f in "${BUILD_DIR}/Prolog/sssp.pl" \
         "${BUILD_DIR}/Lisp/sssp.lisp"; do
    if grep -qP '\t' "$f"; then
        echo "ERRORE: $f contiene TAB!"
        grep -nP '\t' "$f"
        ERRORS=$((ERRORS + 1))
    fi
done

# Verifica niente moduli Prolog
if grep -q ':- module' "${BUILD_DIR}/Prolog/sssp.pl"; then
    echo "ERRORE: sssp.pl contiene moduli Prolog!"
    ERRORS=$((ERRORS + 1))
fi

# Verifica niente package Lisp
if grep -qi 'defpackage\|in-package' \
        "${BUILD_DIR}/Lisp/sssp.lisp"; then
    echo "ERRORE: sssp.lisp contiene package!"
    ERRORS=$((ERRORS + 1))
fi

# Verifica caricamento Prolog
echo "--- Test caricamento Prolog ---"
if swipl -l "${BUILD_DIR}/Prolog/sssp.pl" \
         -g halt 2>&1 | grep -qi "error"; then
    echo "ERRORE: sssp.pl non si carica in SWI-Prolog!"
    ERRORS=$((ERRORS + 1))
else
    echo "OK: sssp.pl caricato con successo."
fi

# Verifica caricamento Lisp
echo "--- Test caricamento Lisp ---"
if sbcl --noinform \
        --load "${BUILD_DIR}/Lisp/sssp.lisp" \
        --quit 2>&1 | grep -qi "error"; then
    echo "ERRORE: sssp.lisp non si carica in SBCL!"
    ERRORS=$((ERRORS + 1))
else
    echo "OK: sssp.lisp caricato con successo."
fi

# Verifica commento iniziale
if ! head -5 "${BUILD_DIR}/Prolog/sssp.pl" | \
     grep -q "928744"; then
    echo "ERRORE: sssp.pl manca matricola in testa!"
    ERRORS=$((ERRORS + 1))
fi
if ! head -5 "${BUILD_DIR}/Lisp/sssp.lisp" | \
     grep -q "928744"; then
    echo "ERRORE: sssp.lisp manca matricola in testa!"
    ERRORS=$((ERRORS + 1))
fi

# Verifica struttura directory
echo "--- Struttura directory ---"
find "${BUILD_DIR}" -type f | sort | \
    sed "s|${DIR_BASE}/||"
echo ""

if [ "$ERRORS" -gt 0 ]; then
    echo ""
    echo "!!! $ERRORS ERRORI TROVATI — CORREGGI PRIMA !!!"
    echo "Packaging ANNULLATO."
    rm -rf "${BUILD_DIR}"
    exit 1
fi

# --------------------------------------------------------
# Creazione ZIP
# --------------------------------------------------------
echo "=== Creazione ZIP ==="
cd "${DIR_BASE}"
zip -r "${ZIP_FILE}" "${NOME}/"
echo ""
echo "=== FATTO ==="
echo "File creato: ${ZIP_FILE}"
echo ""
echo "Contenuto dello ZIP:"
unzip -l "${ZIP_FILE}"

# Pulizia directory temporanea (lo zip basta)
rm -rf "${BUILD_DIR}"

echo ""
echo "Pronto per la consegna su Moodle!"
```

### 13.2 Come usare lo script

1. Assicurati che i file sorgenti siano nelle posizioni corrette:
   - `Prolog/sssp.pl`
   - `Lisp/sssp.lisp`
2. Esegui: `bash package.sh`
3. Lo script valida tutto automaticamente e crea il .zip.
4. Se ci sono errori, li stampa e si ferma — correggi e riesegui.
5. Il file .zip risultante è pronto per il caricamento su Moodle.

### 13.3 Dopo il packaging

- **Verifica manuale finale:** scompatta lo .zip in una
  directory temporanea e ricarica i file da zero per
  assicurarti che funzionino.
  ```bash
  mkdir /tmp/test_consegna && cd /tmp/test_consegna
  unzip /path/to/Horgan_Daniel_Vincent_928744_SSSP_LP_202602.zip
  cd Horgan_Daniel_Vincent_928744_SSSP_LP_202602
  swipl -l Prolog/sssp.pl -g "
      new_graph(g),
      new_arc(g, s, t, 10), new_arc(g, s, y, 5),
      new_arc(g, t, x, 1), new_arc(g, t, y, 2),
      new_arc(g, x, z, 4), new_arc(g, y, t, 3),
      new_arc(g, y, x, 9), new_arc(g, y, z, 2),
      new_arc(g, z, s, 7), new_arc(g, z, x, 6),
      dijkstra_sssp(g, s),
      shortest_path(g, s, x, P),
      write(P), nl, halt."
  # Atteso: [arc(g,s,y,5),arc(g,y,t,3),arc(g,t,x,1)]
  ```
- **Elimina** `package.sh`, file di test, e qualsiasi
  artefatto temporaneo dalla directory di lavoro.
  Solo `Prolog/`, `Lisp/`, e i file di spec devono restare.

---

## 14. RIFERIMENTI

- [CLR+09] Cormen, Leiserson, Rivest, Stein —
  _Introduction to Algorithms_, 3rd Ed., MIT Press, 2009
  (Cap. 6: Heapsort / Cap. 24.3: Dijkstra)
- [SW11] Sedgewick, Wayne — _Algorithms_, 4th Ed.,
  Addison Wesley, 2011
  (Cap. 2.4: Priority Queues / Cap. 4.4: Shortest Paths)
