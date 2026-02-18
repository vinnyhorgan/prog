# Chiarimenti Forum

## 1. Dubbi Progetto SSSP

**D:** Buongiorno prof, avrei tre domande riguardo il progetto SSSP.

1. Il peso degli archi può essere un numero double anche in Lisp? Nella consegna, nel paragrafo "Grafi in Prolog" a pagina 1, c'è l'esempio "arc(v, w, 4.2)", ma per quanto riguarda la parte di Lisp non ci sono esempi.

2. Nel caso in cui il peso di un arco non venga specificato, assumiamo che valga 1 o 0?

3. Quando dobbiamo allargare l'heap (in heap-insert), per evitare di salvare l'heap corrente e ricopiarlo in un nuovo heap di capacità più grande, si può usare la keyword ":adjustable t" in "make-array" nel predicato new-heap in modo tale da usare il predicato adjust-array (in heap-insert)?

**R:** Buonasera
1. si, il peso può essere un float.
2. assumete che sia 1
3. si; attenzione a usare le funzioni corrette per manipolare gli array aggiustabili.

## 2. Progetto SSSP - Prolog

**D:** Buongiorno, volevo chiedere un chiarimento per quanto riguarda le Avverteze del progetto.

Sul testo c'è scritto di evitare costrutti "imperativi" e assegnamenti non esplicitamente permessi. Con questo si intende che per esempio codice di questo tipo non è permesso?

```prolog
heap(H, Size),
NewPos is Size + 1,
```

o con quelle indicazioni si intende altro ancora?

**R:** Buonasera

in Prolog, quello non è un costrutto imperativo. In generale, in Prolog dovete evitare i condizionali '->' e le disgiunzioni ';'.

## 3. Progetto SSSP

**D:** nell'API dei grafi new_vertex(G, V) e new_arc(G, U, V, W) , se un grafo/vertice non esistesse al momento della chiamata dovranno fallire o li creiamo implicitamente?

**R:** Buonasera

La semantica dei due predicati è proprio quella di creare vertici ed archi qualora non esistano.
