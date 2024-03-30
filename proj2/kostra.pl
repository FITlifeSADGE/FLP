start :-
    process_stdin,
    findall(X-Y, connected(X, Y), AllEdges),
    writeln('Všechny hrany:'),
    writeln(AllEdges),
    list_spanning_trees(AllEdges, Trees), % Použijte správnou proměnnou
    writeln('Všechny kostry grafu:'),
    print_trees(Trees),
    writeln('Big gud, funguje ti to.').

% Pomocná funkce pro tisk kostr
print_trees([]).
print_trees([T|Ts]) :-
    writeln(T),
    print_trees(Ts).

% Zde přidejte logiku pro generování a výpis všech kostr
list_spanning_trees(Edges, Trees) :-
    findall(Tree, generate_spanning_tree(Edges, Tree), Trees).

process_stdin :-
    current_input(Stream),
    process_stream(Stream).

process_stream(Stream) :-
    \+ at_end_of_stream(Stream),
    read_line_to_codes(Stream, Line),
    process_line(Line),
    process_stream(Stream).
process_stream(_).

process_line(Line) :-
    string_codes(Str, Line),
    split_string(Str, " ", "", [A, B]),
    assertz(connected(A, B)).

% Tady přidáváme rozšíření
is_connected(X, Y) :- connected(X, Y).
is_connected(X, Y) :- connected(X, Z), is_connected(Z, Y).

check_and_print_connected(X, Y) :-
    is_connected(X, Y),
    format('Prvek ~w je spojen s prvkem ~w.~n', [X, Y]), !.
check_and_print_connected(X, Y) :-
    \+ is_connected(X, Y),
    format('Prvek ~w není spojen s prvkem ~w.~n', [X, Y]).

% Funkce pro ověření, že přidání hrany nevytváří cyklus
not_creates_cycle(X, Y, Visited) :-
    not(member(X, Visited)),
    (connected(X, Y) ; connected(Y, X)),
    !, fail.
not_creates_cycle(X, Y, Visited) :-
    not(member(X, Visited)),
    (   connected(X, Z) ; connected(Z, X) ),
    not_creates_cycle(Z, Y, [X|Visited]).
not_creates_cycle(_, _, _).

% Generování kostry bez cyklů pomocí backtrackingu
generate_spanning_tree(Edges, SpanningTree) :-
    findall(X-Y, connected(X, Y), AllEdges),
    generate_tree(AllEdges, Edges, [], SpanningTree).

generate_tree(_, [], SpanningTree, SpanningTree).
generate_tree(AllEdges, [X-Y|RestEdges], CurrentTree, SpanningTree) :-
    (member(X-Y, AllEdges) ; member(Y-X, AllEdges)),
    not_creates_cycle(X, Y, []),
    generate_tree(AllEdges, RestEdges, [X-Y|CurrentTree], SpanningTree).
generate_tree(AllEdges, [_|RestEdges], CurrentTree, SpanningTree) :-
    generate_tree(AllEdges, RestEdges, CurrentTree, SpanningTree).