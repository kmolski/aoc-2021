:- use_module(library(clpfd)).
:- use_module(library(lists)).
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

% Convert '0' to -1 and '1' to 1, so that the sum of a bit list is
% positive if it has more '1' bits, or negative if it has more '0' bits.
code_to_num(Code, Num) :- (Code = 48 -> Num = -1; Code = 49 -> Num = 1; fail).
choose_bit_p1(List, Result) :-
    sum_list(List, Sum),
    (Sum > 0 -> Result = 1; Sum < 0 -> Result = 0; fail).

bit_negate(Bit, Result) :- (Bit = 0 -> Result = 1; Bit = 1 -> Result = 0; fail).

bit_list_to_int([], 0).
bit_list_to_int([B | R], I) :- bit_list_to_int(R, IRest), I is B + 2 * IRest.

solve_part_1(Cols, Result) :-
    maplist(choose_bit_p1, Cols, Bits),
    reverse(Bits, GammaBits), % Reverse bits to get LSB at head
    bit_list_to_int(GammaBits, Gamma),
    maplist(bit_negate, GammaBits, EpsilonBits), % Epsilon is bit-negated Gamma
    bit_list_to_int(EpsilonBits, Epsilon),
    Result is (Gamma * Epsilon).

num_to_bit(Num, Bit) :- (Num = 1 -> Bit = 1; Num = -1 -> Bit = 0; fail).

get_bit_oxy(Column, Result) :-
    sum_list(Column, Sum),
    (Sum >= 0 -> Result = 1; Sum < 0 -> Result = -1).

get_bit_co2(Column, Result) :-
    sum_list(Column, Sum),
    (Sum < 0 -> Result = 1; Sum >= 0 -> Result = -1).

satisfies_bit_criteria(Bit, _, [Bit]).
satisfies_bit_criteria(Bit, BitPos, Row) :- nth0(BitPos, Row, Bit).

solve_oxy([Rating], _, Rating).
solve_oxy(Rows, BitPos, OxyRating) :-
    transpose(Rows, Cols),
    nth0(BitPos, Cols, NCol),
    get_bit_oxy(NCol, Bit),
    include(satisfies_bit_criteria(Bit, BitPos), Rows, Rest),
    NextPos is BitPos + 1,
    solve_oxy(Rest, NextPos, OxyRating).

solve_co2([Rating], _, Rating).
solve_co2(Rows, BitPos, Co2Rating) :-
    transpose(Rows, Cols),
    nth0(BitPos, Cols, NCol),
    get_bit_co2(NCol, Bit),
    include(satisfies_bit_criteria(Bit, BitPos), Rows, Rest),
    NextPos is BitPos + 1,
    solve_co2(Rest, NextPos, Co2Rating).

row_to_rating(Row, Rating) :-
    maplist(num_to_bit, Row, RBits), % Numeric value (-1, 1) to bit conversion
    reverse(RBits, Bits), % Reverse bits to get LSB at head
    bit_list_to_int(Bits, Rating).

solve_part_2(Rows, Result) :-
    solve_oxy(Rows, 0, OxyRow),
    row_to_rating(OxyRow, OxyRating),
    solve_co2(Rows, 0, Co2Row),
    row_to_rating(Co2Row, Co2Rating),
    Result is (OxyRating * Co2Rating).

main :-
    current_prolog_flag(argv, [Filename | _]),
    file_lines(Filename, Lines),
    maplist(maplist(code_to_num), Lines, Rows),
    transpose(Rows, Cols),
    solve_part_1(Cols, Result1),
    solve_part_2(Rows, Result2),
    format('Part 1: ~q~n', [Result1]),
    format('Part 2: ~q~n', [Result2]),
    halt.
main :-
    halt(1).
