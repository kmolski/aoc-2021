:- use_module(library(clpfd)).
:- initialization(main).

empty([]).

stream_lines(In, Lines) :-
    read_string(In, _, Str),
    split_string(Str, "\n", "", L1),
    maplist(string_codes, L1, L2),
    exclude(empty, L2, Lines).

file_lines(Filename, Lines) :-
    setup_call_cleanup(open(Filename, read, In),
        stream_lines(In, Lines),
        close(In)).

solve_part_1(Lines, Result) :-
    transpose(Lines, Result).

main :-
    current_prolog_flag(argv, Argv),
    Argv = [Filename|_],
    file_lines(Filename, Lines),
    solve_part_1(Lines, Result),
    format('Lines: ~q~n', [Result]),
    halt.
main :-
    halt(1).
