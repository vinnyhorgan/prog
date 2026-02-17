% ============================================
% 1. INSERTION SORT
% ============================================

insertion_sort([], []).
insertion_sort([H|T], Sorted) :-
    insertion_sort(T, SortedT),
    insert(H, SortedT, Sorted).

% Inserisce un elemento in una lista ordinata
insert(X, [], [X]).
insert(X, [H|T], [X,H|T]) :- X =< H.
insert(X, [H|T], [H|Rest]) :- X > H, insert(X, T, Rest).

% ============================================
% 2. QUICKSORT
% ============================================

quicksort([], []).
quicksort([Pivot|Rest], Sorted) :-
    partition(Pivot, Rest, Smaller, Larger),
    quicksort(Smaller, SortedSmaller),
    quicksort(Larger, SortedLarger),
    append(SortedSmaller, [Pivot|SortedLarger], Sorted).

% Partiziona la lista in elementi minori e maggiori del pivot
partition(_, [], [], []).
partition(Pivot, [H|T], [H|Smaller], Larger) :-
    H =< Pivot,
    partition(Pivot, T, Smaller, Larger).
partition(Pivot, [H|T], Smaller, [H|Larger]) :-
    H > Pivot,
    partition(Pivot, T, Smaller, Larger).

% ============================================
% 3. MERGE SORT
% ============================================

merge_sort([], []).
merge_sort([X], [X]).
merge_sort(List, Sorted) :-
    List = [_,_|_],
    split(List, Left, Right),
    merge_sort(Left, SortedLeft),
    merge_sort(Right, SortedRight),
    merge(SortedLeft, SortedRight, Sorted).

% Divide la lista in due metà
split([], [], []).
split([X], [X], []).
split([X,Y|Rest], [X|Left], [Y|Right]) :-
    split(Rest, Left, Right).

% Merge di due liste ordinate
merge([], L, L).
merge(L, [], L).
merge([H1|T1], [H2|T2], [H1|Rest]) :-
    H1 =< H2,
    merge(T1, [H2|T2], Rest).
merge([H1|T1], [H2|T2], [H2|Rest]) :-
    H1 > H2,
    merge([H1|T1], T2, Rest).

% ============================================
% 4. BUBBLE SORT
% ============================================

bubble_sort(List, Sorted) :-
    bubble_pass(List, List1, false, Swapped),
    bubble_sort_continue(List1, Sorted, Swapped).

% Continua se ci sono stati scambi, altrimenti termina
bubble_sort_continue(List, Sorted, true) :-
    bubble_sort(List, Sorted).
bubble_sort_continue(List, List, false).

% Effettua un passaggio completo sulla lista
bubble_pass([], [], Flag, Flag).
bubble_pass([X], [X], Flag, Flag).
bubble_pass([X,Y|Rest], [X|T], _, FinalFlag) :-
    X =< Y,
    bubble_pass([Y|Rest], T, false, FinalFlag).
bubble_pass([X,Y|Rest], [Y|T], _, FinalFlag) :-
    X > Y,
    bubble_pass([X|Rest], T, true, FinalFlag).

% ============================================
% ESEMPI DI UTILIZZO
% ============================================

% ?- insertion_sort([3,1,4,1,5,9,2,6], Sorted).
% Sorted = [1,1,2,3,4,5,6,9].

% ?- quicksort([3,1,4,1,5,9,2,6], Sorted).
% Sorted = [1,1,2,3,4,5,6,9].

% ?- merge_sort([3,1,4,1,5,9,2,6], Sorted).
% Sorted = [1,1,2,3,4,5,6,9].

% ?- bubble_sort([3,1,4,1,5,9,2,6], Sorted).
% Sorted = [1,1,2,3,4,5,6,9].