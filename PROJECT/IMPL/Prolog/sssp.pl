% Daniel Vincent Horgan - 928744

:- dynamic graph/1.
:- dynamic vertex/2.
:- dynamic arc/4.
:- dynamic heap/2.
:- dynamic heap_entry/4.
:- dynamic distance/3.
:- dynamic visited/2.
:- dynamic previous/3.

% --- SEZIONE MINHEAP ---

new_heap(H) :-
    heap(H, _),
    !.
new_heap(H) :-
    assert(heap(H, 0)),
    !.

delete_heap(H) :-
    retractall(heap_entry(H, _, _, _)),
    retractall(heap(H, _)),
    !.

heap_size(H, S) :-
    heap(H, S).

empty(H) :-
    heap(H, 0).

not_empty(H) :-
    heap(H, S),
    S > 0.

head(H, K, V) :-
    heap(H, S),
    S > 0,
    heap_entry(H, 1, K, V).

insert(H, K, V) :-
    new_heap(H),
    heap(H, S),
    Pos is S + 1,
    update_heap_size(H, Pos),
    assert(heap_entry(H, Pos, K, V)),
    heapify_up(H, Pos),
    !.

extract(H, K, V) :-
    heap(H, S),
    S > 0,
    heap_entry(H, 1, K, V),
    extract_root(H, S),
    !.

modify_key(H, NewKey, OldKey, V) :-
    heap_entry(H, Pos, OldKey, V),
    retract(heap_entry(H, Pos, OldKey, V)),
    assert(heap_entry(H, Pos, NewKey, V)),
    reheap_after_modify(H, Pos, NewKey, OldKey),
    !.

list_heap(H) :-
    listing(heap_entry(H, _, _, _)).

update_heap_size(H, NewSize) :-
    retract(heap(H, _)),
    assert(heap(H, NewSize)).

extract_root(H, 1) :-
    retract(heap_entry(H, 1, _, _)),
    update_heap_size(H, 0),
    !.
extract_root(H, S) :-
    S > 1,
    LastPos is S,
    heap_entry(H, LastPos, LastK, LastV),
    retract(heap_entry(H, 1, _, _)),
    retract(heap_entry(H, LastPos, _, _)),
    assert(heap_entry(H, 1, LastK, LastV)),
    NewSize is S - 1,
    update_heap_size(H, NewSize),
    heapify_down(H, 1),
    !.

reheap_after_modify(H, Pos, NewKey, OldKey) :-
    % Se la chiave scende, risalgo come nella classica decrease-key.
    NewKey < OldKey,
    heapify_up(H, Pos),
    !.
reheap_after_modify(H, Pos, NewKey, OldKey) :-
    % Se invece cresce, devo scendere per ripristinare la heap property.
    NewKey > OldKey,
    heapify_down(H, Pos),
    !.
reheap_after_modify(_H, _Pos, _NewKey, _OldKey) :-
    !.

swap_entries(H, P1, P2) :-
    heap_entry(H, P1, K1, V1),
    heap_entry(H, P2, K2, V2),
    retract(heap_entry(H, P1, K1, V1)),
    retract(heap_entry(H, P2, K2, V2)),
    assert(heap_entry(H, P1, K2, V2)),
    assert(heap_entry(H, P2, K1, V1)),
    !.

heapify_up(_H, 1) :-
    !.
heapify_up(H, Pos) :-
    Pos > 1,
    Parent is Pos // 2,
    heap_entry(H, Pos, Key, _),
    heap_entry(H, Parent, ParentKey, _),
    Key < ParentKey,
    swap_entries(H, Pos, Parent),
    heapify_up(H, Parent),
    !.
heapify_up(_H, _Pos) :-
    !.

heapify_down(H, Pos) :-
    heap(H, Size),
    smallest_child(H, Pos, Size, 0),
    !.
heapify_down(H, Pos) :-
    heap(H, Size),
    smallest_child(H, Pos, Size, Child),
    Child > 0,
    heap_entry(H, Pos, Key, _),
    heap_entry(H, Child, ChildKey, _),
    ChildKey < Key,
    swap_entries(H, Pos, Child),
    heapify_down(H, Child),
    !.
heapify_down(_H, _Pos) :-
    !.

smallest_child(_H, Pos, Size, 0) :-
    Left is Pos * 2,
    Left > Size,
    !.
smallest_child(_H, Pos, Size, Left) :-
    Left is Pos * 2,
    Right is Left + 1,
    Right > Size,
    !.
smallest_child(H, Pos, _Size, Left) :-
    Left is Pos * 2,
    Right is Left + 1,
    heap_entry(H, Left, LeftKey, _),
    heap_entry(H, Right, RightKey, _),
    LeftKey =< RightKey,
    !.
smallest_child(_H, Pos, _Size, Right) :-
    Left is Pos * 2,
    Right is Left + 1,
    !.

% --- SEZIONE GRAFI ---

new_graph(G) :-
    graph(G),
    !.
new_graph(G) :-
    assert(graph(G)),
    !.

delete_graph(G) :-
    retractall(graph(G)),
    retractall(vertex(G, _)),
    retractall(arc(G, _, _, _)),
    retractall(distance(G, _, _)),
    retractall(visited(G, _)),
    retractall(previous(G, _, _)),
    !.

new_vertex(G, V) :-
    new_graph(G),
    vertex(G, V),
    !.
new_vertex(G, V) :-
    new_graph(G),
    assert(vertex(G, V)),
    !.

vertices(G, Vs) :-
    findall(V, vertex(G, V), Vs).

list_vertices(G) :-
    listing(vertex(G, _)).

new_arc(G, U, V) :-
    new_arc(G, U, V, 1).

new_arc(G, U, V, Weight) :-
    number(Weight),
    Weight >= 0,
    new_vertex(G, U),
    new_vertex(G, V),
    retractall(arc(G, U, V, _)),
    assert(arc(G, U, V, Weight)),
    !.

arcs(G, Es) :-
    findall(arc(G, U, V, W), arc(G, U, V, W), Es).

neighbors(G, V, Ns) :-
    vertex(G, V),
    findall(arc(G, V, N, W), arc(G, V, N, W), Ns).

list_arcs(G) :-
    listing(arc(G, _, _, _)).

list_graph(G) :-
    list_vertices(G),
    list_arcs(G).

% --- SEZIONE SSSP ---

change_distance(G, V, NewDist) :-
    retractall(distance(G, V, _)),
    assert(distance(G, V, NewDist)),
    !.

change_previous(G, V, U) :-
    retractall(previous(G, V, _)),
    assert(previous(G, V, U)),
    !.

dijkstra_sssp(G, Source) :-
    graph(G),
    vertex(G, Source),
    cleanup_sssp_state(G),
    vertices(G, Vs),
    init_distances(G, Vs, Source),
    TempHeap = sssp_heap(G),
    delete_heap(TempHeap),
    new_heap(TempHeap),
    init_heap_entries(TempHeap, Vs, Source),
    dijkstra_loop(G, TempHeap),
    delete_heap(TempHeap),
    !.

cleanup_sssp_state(G) :-
    retractall(distance(G, _, _)),
    retractall(visited(G, _)),
    retractall(previous(G, _, _)).

init_distances(_G, [], _Source) :-
    !.
init_distances(G, [V | Rest], Source) :-
    V == Source,
    change_distance(G, V, 0),
    init_distances(G, Rest, Source),
    !.
init_distances(G, [V | Rest], Source) :-
    V \== Source,
    change_distance(G, V, inf),
    init_distances(G, Rest, Source),
    !.

init_heap_entries(_H, [], _Source) :-
    !.
init_heap_entries(H, [V | Rest], Source) :-
    V == Source,
    insert(H, 0, V),
    init_heap_entries(H, Rest, Source),
    !.
init_heap_entries(H, [V | Rest], Source) :-
    V \== Source,
    insert(H, inf, V),
    init_heap_entries(H, Rest, Source),
    !.

dijkstra_loop(_G, H) :-
    empty(H),
    !.
dijkstra_loop(G, H) :-
    extract(H, D, V),
    dijkstra_step(G, H, D, V),
    dijkstra_loop(G, H).

dijkstra_step(G, _H, _D, V) :-
    % Voce vecchia: puo' capitare se una chiave e' stata ridotta dopo.
    visited(G, V),
    !.
dijkstra_step(G, H, D, V) :-
    assert(visited(G, V)),
    neighbors(G, V, Ns),
    relax_neighbors(G, H, D, V, Ns),
    !.

relax_neighbors(_G, _H, _D, _V, []) :-
    !.
relax_neighbors(G, H, D, V, [arc(G, V, N, W) | Rest]) :-
    relax_neighbor(G, H, D, V, N, W),
    relax_neighbors(G, H, D, V, Rest).

relax_neighbor(G, _H, _D, _V, N, _W) :-
    visited(G, N),
    !.
relax_neighbor(G, H, D, V, N, W) :-
    distance(G, N, OldDist),
    NewDist is D + W,
    NewDist < OldDist,
    change_distance(G, N, NewDist),
    change_previous(G, N, V),
    modify_key(H, NewDist, OldDist, N),
    !.
relax_neighbor(_G, _H, _D, _V, _N, _W) :-
    !.

shortest_path(G, Source, Source, []) :-
    vertex(G, Source),
    !.
shortest_path(G, Source, V, Path) :-
    Source \== V,
    build_shortest_path(G, Source, V, [], Path),
    Path \= [].

build_shortest_path(_G, Source, Source, Acc, Acc) :-
    !.
build_shortest_path(G, Source, V, Acc, Path) :-
    % Risalgo coi predecessori e accumulo in testa, cosi' evito reverse/2.
    previous(G, V, U),
    arc(G, U, V, W),
    build_shortest_path(
        G,
        Source,
        U,
        [arc(G, U, V, W) | Acc],
        Path
    ).
