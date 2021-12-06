:- use_module(library(clpfd)).
:- use_module(library(lists)).
:- initialization(main).

string_number(Str, Num) :- number_string(Num, Str).

stream_fields(In, Fields) :-
    read_string(In, _, Str),
    split_string(Str, ",", "\n", L),
    maplist(string_number, L, Fields).

file_fields(Filename, Fields) :-
    setup_call_cleanup(open(Filename, read, In),
        stream_fields(In, Fields),
        close(In)).

% S - simulation start time,            E - simulation end time
% C - days until new lanternfish spawn, D - number of descendants
% N prefix - new value
count_fish_and_descendants(0, _, 1).
count_fish_and_descendants(E, 0, D) :-
    NE is E - 1,
    count_fish_and_descendants(NE, 8, CR), % count descendants for child
    count_fish_and_descendants(NE, 6, TD), % count descendants for this
    D is TD + CR.

count_fish_and_descendants(E, C, D) :-
    NE is E - 1, NC is C - 1, count_fish_and_descendants(NE, NC, D).

solve(Fields, E, Result) :-
    maplist(count_fish_and_descendants(E), Fields, Counts),
    sum_list(Counts, Result).

main :-
    current_prolog_flag(argv, [Filename | _]),
    file_fields(Filename, Fields),
    solve(Fields, 80, Result1),
    format('Part 1: ~q~n', [Result1]),
    solve(Fields, 256, Result2),
    format('Part 2: ~q~n', [Result2]),
    halt.
main :-
    halt(1).
