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

/* Odstraní druhý prvek z každého řádku (po vsuput mám [A, ,B], chci jen [A,B]) */
remove_second([], []).
remove_second([[A, _, B | Rest]|Tail], [[A, B | Rest]|ResultTail]) :-
    remove_second(Tail, ResultTail).




/* Dostane seznam grafů a vrací řádky, které neobsahují cykly */
get_line([], _, []).  
get_line([A|B], Sorted, Result) :-
    get_line(B, Sorted, TailResult),  % Zkontroluji zbytek grafl
    find_cycle(A, Sorted, Cycle),  % Kontrola grafu A
    (
        \+ Cycle,  % Graf A neměl cyklus
        Result = [A|TailResult];   % Přidá graf k výsledku
        Result = TailResult  % Graf A měl cyklus -> nepřidávám
    ).

/* Dostane řádek grafu a seznam vrcholů, vrací Bool, zda řádek obsahuje všechny vrcholy */
find_cycle(_, [], false).  % Když není žádný vrchol k ověření, vrací false
find_cycle(Row, [A|B], Cycle) :-
    remove_cycle([A], Row, CycleFound),  % Zkontroluje, zda je cyklus s vrcholem A
    (
        \+ CycleFound,  % Pokud není cyklus s vrcholem A
        find_cycle(Row, B, Cycle);  % Pokračuje s dalšími vrcholy
        Cycle = true  % Nastaví, že řádek obsahuje cyklus
    ).


% Vrací true, pokud řádek obsahuje cyklus, jinak false.
remove_cycle([A|B], Row, Cycle) :-
    process_vertex(A, B, Row, Cycle). % spustí prohledávání vrcholů

% Postupně prohledám vrcholy
process_vertex(Vertex, Visited, Row, Cycle) :-
    remove_specific_neighbor(Vertex, Row, Result),  % Odstraním všechny hrany s vrcholem A a zbytek uložím do Result
    find_neighbour(Vertex, Row, Neighbors),  % Najdu z původního řádku všechny sousední vrcholy vrcholu A
    check_cycle(Vertex, Visited, Neighbors, Result, Cycle).

check_cycle(Vertex, Visited, Neighbors, Result, Cycle) :-
    (   \+ memberchk(Vertex, Visited) ->  % Pokud jsem ještě nebyl ve vrcholu A, hledám dál ve zbytku (bez hran s vrcholem A)
        branching([Vertex|Visited], Neighbors, Result, Cycle);   
        Cycle = true  % Už jsem byl ve vrcholu A, teda mám cyklus a vracím true
    ).

/*dostane visited, neighbours, row, vraci Bool*/
branching(_, [], _, false). % Pokud jsem už prošel všechny sousedy, vracím false.
branching(Visited, [A|B], Row, Cycle) :- % Mám seznam navštívených vrcholů, sousedy, jeden řádek grafu
    check_specific_neighbor(A, Visited, B, Row, Cycle). 

check_specific_neighbor(Vertex, Visited, RemainingNeighbors, Row, Cycle) :-
    remove_cycle([Vertex|Visited], Row, CycleFound),  % Zkoumá cyklus s vrcholem Vertex jako aktuálním vrcholem
    check_for_cycle(Vertex, Visited, RemainingNeighbors, Row, CycleFound, Cycle). % Podle výsledku remove_cycle rozhodnu dál

check_for_cycle(Vertex, Visited, RemainingNeighbors, Row, false, Cycle) :-
    branching([Vertex|Visited], RemainingNeighbors, Row, Cycle).  % CycleFound byl false, hledám dál
check_for_cycle(_, _, _, _, true, true). % CycleFound byl true, vracím true


% Získá kostru grafu a jeden vrchol, odstraní všechny hrany, které vedou do tohoto vrcholu.
remove_specific_neighbor(_, [], []).  % Prázdný seznam na vstupu vrací prázdný seznam.
remove_specific_neighbor(Neigh, [[Neigh, _]|Rest], Result) :- % Pokud mám na vstupu například 'a' a hranu 'a-b', tuto hranu odstraním
    remove_specific_neighbor(Neigh, Rest, TailResult), % Rekurzivně hledám sousedy vrcholu ve zbytku seznamu
    Result = TailResult. % Do výsledku nepřidávám momentální hranu.
remove_specific_neighbor(Neigh, [[_, Neigh]|Rest], Result) :- % Pokud mám na vstupu například 'a' a hranu 'b-a', tuto hranu odstraním
    remove_specific_neighbor(Neigh, Rest, TailResult), % Rekurzivně hledám sousedy vrcholu ve zbytku seznamu
    Result = TailResult. % Do výsledku nepřidávám momentální hranu.
remove_specific_neighbor(Neigh, [[X, Y]|Rest], Result) :- % Pokud mám na vstupu například 'a' a hranu 'b-c', hranu 'b-c' přidám do výsledku
    remove_specific_neighbor(Neigh, Rest, TailResult), % Rekurzivně hledám sousedy vrcholu ve zbytku seznamu
    Result = [[X, Y]|TailResult]. % Do výsledku přidávám momentální hranu.

% Získá kostru grafu a jeden vrchol, vrátí všechny jeho sousedy.
find_neighbour(_, [], []).  % Prázdný seznam na vstupu vrací prázdný seznam.
find_neighbour(A, [[A, X]|Rest], Result) :- % Pokud mám na vstupu například 'a' a hranu 'a-b', do výsledku přidám 'b'
    find_neighbour(A, Rest, TailResult), % Rekurzivně hledám sousedy ve zbytku hran.
    Result = [X|TailResult]. % Do výslednýho seznamu dám vždy souseda
find_neighbour(A, [[X, A]|Rest], Result) :- % Pokud mám na vstupu například 'a' a hranu 'b-a', do výsledku přidám 'b'
    find_neighbour(A, Rest, TailResult), % Rekurzivně hledám sousedy ve zbytku hran.
    Result = [X|TailResult]. % Do výslednýho seznamu dám vždy souseda
find_neighbour(A, [[X, Y]|Rest], Result) :- % Pokud má mna vstupu například 'a' a hranu 'b-c', do výsledku nic nepřidám
    find_neighbour(A, Rest, Result). % Rekurzivně hledám sousedy ve zbytku hran.


printTree([]).
printTree([[A,B], [C,D]|Tail]) :- format('~w-~w ', [A, B]), printTree([[C,D]|Tail]).
printTree([[A,B]|Tail]) :- format('~w-~w\n', [A, B]), printTree(Tail).


start :-
		prompt(_, ''),
		read_lines(LL),
        remove_second(LL, NewLL), % odstraní druhý prvek
        flatten(NewLL, Flat), % převede na jeden list
        sort(Flat, Sorted), % odstraní duplikáty
        length(Sorted, Length), % zjistí počet prvků
        findall_combinations((Length-1), NewLL, Comb), % vytvoří kombinace o délce N - 1
        get_line(Comb, Sorted, KOKOT), % zjistí, jestli je v grafu cyklus
        maplist(printTree, KOKOT) ; true.
        