/** FLP 2020
Toto je ukazkovy soubor zpracovani vstupu v prologu.
Tento soubor muzete v projektu libovolne pouzit.

autor: Martin Hyrs, ihyrs@fit.vutbr.cz
preklad: swipl -q -g start -o flp19-log -c input2.pl
*/
/** cte radky ze standardniho vstupu, konci na LF nebo EOF */
read_line(L,C) :-
	get_char(C),
	(isEOFEOL(C), L = [], !;
		read_line(LL,_),% atom_codes(C,[Cd]),
		[C|LL] = L).


/** testuje znak na EOF nebo LF */
isEOFEOL(C) :-
	C == end_of_file;
	(char_code(C,Code), Code==10).


read_lines(Ls) :-
	read_line(L,C),
	( C == end_of_file, Ls = [] ;
	  read_lines(LLs), Ls = [L|LLs]
	).
/** vypise seznam radku (kazdy radek samostatne) */
write_lines2([]).
write_lines2([H|T]) :- writeln(H), write_lines2(T). %(writeln je "knihovni funkce")

/** konec souboru input2.pl */

findall_combinations(Length, List, Combinations) :-
    findall(Combo, comb(Length, List, Combo), Combinations).

comb(0,_,[]).
comb(N,[X|T],[X|Comb]) :-
    N>0,
    N1 is N-1,
    comb(N1,T,Comb).
comb(N,[_|T],Comb) :-
    N>0,
    comb(N,T,Comb).

remove_second([], []).
remove_second([[A, _, B | Rest]|Tail], [[A, B | Rest]|ResultTail]) :-
    remove_second(Tail, ResultTail).

%% naše řešení %%%



/*ZKUSIT PŘEPSAT ABY TO NEBYLO SUS*/

replace_brackets([], []). % Pokud je vstup prázdný seznam, výstup bude také prázdný seznam.

replace_brackets([InnerList|RestInput], [ReplacedInnerList|RestOutput]) :-
    % Nahradíme vnitřní hranaté závorky v InnerList za kulaté závorky v ReplacedInnerList
    replace_brackets_in_list(InnerList, ReplacedInnerList),
    % Rekurzivně pokračujeme s dalšími prvky vstupního seznamu.
    replace_brackets(RestInput, RestOutput).

replace_brackets_in_list([], []). % Pokud je vstup prázdný seznam, výstup bude také prázdný seznam.

replace_brackets_in_list([InnerTuple|RestInput], [ReplacedInnerTuple|RestOutput]) :-
    % Nahradíme vnitřní hranaté závorky v InnerTuple za kulaté závorky v ReplacedInnerTuple
    replace_brackets_in_tuple(InnerTuple, ReplacedInnerTuple),
    % Rekurzivně pokračujeme s dalšími prvky vstupního seznamu.
    replace_brackets_in_list(RestInput, RestOutput).

replace_brackets_in_tuple([], []). % Pokud je vstup prázdný seznam, výstup bude také prázdný seznam.

replace_brackets_in_tuple([X,Y], (X,Y)) :- !. % Pokud máme dvojici [X,Y], nahradíme ji (X,Y).

% Rekurzivně pokračujeme s dalšími prvky n-tice.
replace_brackets_in_tuple([Head|Tail], (Head,ReplacedTail)) :-
    replace_brackets_in_tuple(Tail, ReplacedTail).

/* Dostane seznam grafů a vrací řádky, které neobsahují cykly */
get_line([], _, []).
get_line([H|T], Sorted, KOKOT) :- get_line(T, Sorted, KOMAR), 
find_vertices(H, Sorted, JANEVIM),
    ( 
        \+ JANEVIM, KOKOT = [H|KOMAR];
        KOKOT = KOMAR
    ).

% dostane řádek grafu, seznam vrcholů a vrací, zda obsahuje všechny vrcholy
find_vertices(_, [], false).
find_vertices(Row, [A|B], JANEVIM) :- remove_cycle([A], Row, Picus),
    (   \+ Picus, find_vertices(Row, B, JANEVIM);
        JANEVIM = true
        
    ).


/*Zjistí, jestli je A v B(Visited)*/
is_visited(A, [A|_]) :- !.
is_visited(A, [_|T]) :- is_visited(A, T).

/*řádek, seznam visited -> vrací Bool*/
% Vrací true, pokud řádek obsahuje cyklus, jinak false.
remove_cycle([A|B], Row, Picus) :-   
    remove_specific_neighbor(A, Row, Res),
    find_neighbour(A, Row, Neighbour),
    (   \+ is_visited(A, B), branching([A|B], Neighbour, Res, Picus);
        Picus = true  
    ).

/*dostane visited, neighbours, row, vraci Bool*/
% provádí prohledávání do hloubky grafu a zjišťuje, zda existuje cyklus.
branching(_, [], _, false).
branching(Visited, [A|B], Row, ResBranching) :- 
    remove_cycle([A|Visited], Row, ResRemove),
    (   \+ ResRemove, branching([A|Visited], B, Row, ResBranching);
        ResBranching = true
    ).

% Získá kostru grafu a jeden vrchol, odstraní všechny hrany, které vedou do tohoto vrcholu.
remove_specific_neighbor(_, [], []).  % Prázdný seznam vstupů vede na prázdný seznam výstupů.
remove_specific_neighbor(Neighbor, [(A,Neighbor)|Arr], Res) :- remove_specific_neighbor(Neighbor, Arr, ResTail), Res = ResTail.
remove_specific_neighbor(Neighbor, [(Neighbor,A)|Arr], Res) :- remove_specific_neighbor(Neighbor, Arr, ResTail), Res = ResTail.
remove_specific_neighbor(Neighbor, [(X,Y)|Arr], [(X,Y)|Res]) :- Neighbor \= Y, Neighbor \= X, remove_specific_neighbor(Neighbor, Arr, Res).

% Získá kostru grafu a jeden vrchol, vrátí všechny jeho sousedy.
find_neighbour(_, [], []).  % Prázdný seznam vstupů vede na prázdný seznam výstupů.
find_neighbour(A, [(A,X)|Arr], Res) :- find_neighbour(A, Arr, ResTail), Res = [X|ResTail].
find_neighbour(A, [(X,A)|Arr], Res) :- find_neighbour(A, Arr, ResTail), Res = [X|ResTail].
find_neighbour(A, [(X,Y)|Arr], Res) :- A \= X, A \= Y, find_neighbour(A, Arr, Res).  % Přidáno pro zachování struktury, kdy A není součástí dvojice.
find_neighbour(A, [X|Arr], Res) :- A \= X, find_neighbour(A, Arr, Res).  % Přidáno pro zachování ostatních prvků, které nejsou dvojice.


% Prints single spanning tree in the specified format
printTree([]).
printTree([(A,B), (C,D)|Tail]) :- format('~w-~w ', [A, B]), printTree([(C,D)|Tail]).
printTree([(A,B)|Tail]) :- format('~w-~w\n', [A, B]), printTree(Tail).

/*PO SEM ZKUSIT PŘEPSAT*/

start :-
		prompt(_, ''),
		read_lines(LL),
        remove_second(LL, NewLL), % odstraní druhý prvek
        flatten(NewLL, Flat), % převede na jeden list
        sort(Flat, Sorted), % odstraní duplikáty
        length(Sorted, Length), % zjistí počet prvků
        findall_combinations((Length-1), NewLL, Comb), % vytvoří kombinace o délce N - 1
        replace_brackets(Comb, Replaced), % nahradí hranaté závorky za kulaté
        get_line(Replaced, Sorted, KOKOT), % zjistí, jestli je v grafu cyklus
        maplist(printTree, KOKOT) ; true.
        