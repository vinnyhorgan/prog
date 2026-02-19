Caricamento in SWI-Prolog:

  swipl -l sssp.pl

Oppure dal prompt:

  ?- [sssp].

Esempio rapido:

  ?- new_graph(g).
  ?- new_arc(g, s, t, 10).
  ?- new_arc(g, s, y, 5).
  ?- dijkstra_sssp(g, s).
  ?- shortest_path(g, s, t, P).

Per test non interattivi:

  swipl -q -l sssp.pl -g "goal1, goal2, halt."
