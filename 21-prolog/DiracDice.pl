:- initialization(main).

:- table dirac_dice/1.
:- table make_move/7.
:- table solve_part2/6.

det_dice(Turn, Roll) :- I is Turn * 3, Roll is 3 * I + 6.

product(A, R) :- findall([X, Y, Z], (member(X, A), member(Y, A), member(Z, A)), R).
dirac_dice(Sums) :- product([1, 2, 3], Rolls), maplist(sum_list, Rolls, Sums).

solve_part1(_, _, Score1, Score2, Turn, R) :- Score1 >= 1000, R is Score2 * (Turn * 3).
solve_part1(_, _, Score1, Score2, Turn, R) :- Score2 >= 1000, R is Score1 * (Turn * 3).
solve_part1(Pos1, Pos2, Score1, Score2, Turn, Result) :-
    0 is mod(Turn, 2),
    det_dice(Turn, Roll),
    NewPos is (Pos1 + Roll - 1) mod 10 + 1,
    NewScore is Score1 + NewPos,
    NewTurn is Turn + 1,
    solve_part1(NewPos, Pos2, NewScore, Score2, NewTurn, Result).
solve_part1(Pos1, Pos2, Score1, Score2, Turn, Result) :-
    1 is mod(Turn, 2),
    det_dice(Turn, Roll),
    NewPos is (Pos2 + Roll - 1) mod 10 + 1,
    NewScore is Score2 + NewPos,
    NewTurn is Turn + 1,
    solve_part1(Pos1, NewPos, Score1, NewScore, NewTurn, Result).

make_move(Pos1, Pos2, Score1, Score2, 0, RollSum, Counter) :-
    NewPos is (Pos1 + RollSum - 1) mod 10 + 1,
    NewScore is Score1 + NewPos,
    solve_part2(NewPos, Pos2, NewScore, Score2, 0, Counter).
make_move(Pos1, Pos2, Score1, Score2, 1, RollSum, Counter) :-
    NewPos is (Pos2 + RollSum - 1) mod 10 + 1,
    NewScore is Score2 + NewPos,
    solve_part2(Pos1, NewPos, Score1, NewScore, 1, Counter).

merge_counters(Counters, Counter) :-
    maplist(nth0(0), Counters, Player1),
    sum_list(Player1, Won1),
    maplist(nth0(1), Counters, Player2),
    sum_list(Player2, Won2),
    Counter = [Won1, Won2].

solve_part2(Pos1, Pos2, Score1, Score2, 0, Counter) :-
    Score1 < 21, dirac_dice(Sums),
    maplist(make_move(Pos1, Pos2, Score1, Score2, 1), Sums, Counters),
    merge_counters(Counters, Counter).
solve_part2(Pos1, Pos2, Score1, Score2, 1, Counter) :-
    Score2 < 21, dirac_dice(Sums),
    maplist(make_move(Pos1, Pos2, Score1, Score2, 0), Sums, Counters),
    merge_counters(Counters, Counter).
solve_part2(_, _, Score1, _, _, R) :- Score1 >= 21, R = [1, 0].
solve_part2(_, _, _, Score2, _, R) :- Score2 >= 21, R = [0, 1].

main :-
    P1Start = 5,
    P2Start = 10,
    solve_part1(P1Start, P2Start, 0, 0, 0, Result1),
    format('Part 1: ~q~n', [Result1]),
    solve_part2(P1Start, P2Start, 0, 0, 1, Counter),
    max_list(Counter, Result2),
    format('Part 2: ~q~n', [Result2]),
    halt.
main :-
    halt(1).
