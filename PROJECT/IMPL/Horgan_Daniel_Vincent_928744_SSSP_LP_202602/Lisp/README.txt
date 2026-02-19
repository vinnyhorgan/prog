Caricamento in Common Lisp:

  (load "sssp.lisp")

Esempio rapido:

  (new-graph 'g)
  (new-arc 'g 's 't 10)
  (new-arc 'g 's 'y 5)
  (sssp-dijkstra 'g 's)
  (sssp-shortest-path 'g 's 't)

Test rapido da shell (SBCL):

  sbcl --noinform --load sssp.lisp --eval "(new-graph 'g)" --quit
