:- initialization(main).

:- table new_fish/3.
:- table count_fish/3.

string_number(Str, Num) :- number_string(Num, Str).

stream_fields(In, Fields) :-
    read_string(In, _, Str),
    split_string(Str, ",", "\n", Fs),
    maplist(string_number, Fs, Fields).

file_fields(Filename, Fields) :-
    setup_call_cleanup(open(Filename, read, In),
        stream_fields(In, Fields),
        close(In)).

% E - days until simulation end, S - amount of spawns left
% D - number of descendants, N prefix - updated value
new_fish(E, I, D) :- NE is E - I * 7, count_fish(NE, 8, D).

count_fish(E, _, 1) :- E =< 0. % a fish can't spawn if there are zero days until end
count_fish(E, 0, D) :-
    NE is E - 1, S is div(NE, 7),
    numlist(0, S, I),
    maplist(new_fish(NE), I, Counts),
    sum_list(Counts, CSum),
    D is 1 + CSum.

% C - days until new spawn
count_fish(E, C, D) :- NE is E - C, count_fish(NE, 0, D).

solve(Fields, E, Result) :-
    maplist(count_fish(E), Fields, Counts), sum_list(Counts, Result).

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
