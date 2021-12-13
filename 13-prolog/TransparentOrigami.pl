:- initialization(main).

line_position(Line, [X, Y]) :-
    split_string(Line, ",", "", [StringX, StringY]),
    number_string(X, StringX),
    number_string(Y, StringY).

line_fold(Line, [Dimension, Coord]) :-
    split_string(Line, "=", "fold along ", [Dimension, StringCoord]),
    number_string(Coord, StringCoord).

stream_pos_folds(In, Positions, Folds) :-
    read_string(In, _, Str),
    re_split("\n\n", Str, [P, _, F]),
    split_string(P, "\n", "\n", PositionLines),
    split_string(F, "\n", "\n", FoldLines),
    maplist(line_position, PositionLines, Positions),
    maplist(line_fold, FoldLines, Folds).

file_pos_folds(Filename, Positions, Folds) :-
    setup_call_cleanup(open(Filename, read, In),
        stream_pos_folds(In, Positions, Folds),
        close(In)).

fold_coordinate("x", Coord, [X, Y], [NX, Y]) :- NX is 2 * Coord - X.
fold_coordinate("y", Coord, [X, Y], [X, NY]) :- NY is 2 * Coord - Y.

to_be_folded("x", Coord, [X, _]) :- X > Coord.
to_be_folded("y", Coord, [_, Y]) :- Y > Coord.

fold(Positions, Dimension, Coord, NewPositions) :-
    partition(to_be_folded(Dimension, Coord), Positions, ToFold, Unchanged),
    maplist(fold_coordinate(Dimension, Coord), ToFold, Folded),
    append(Unchanged, Folded, NP),
    list_to_ord_set(NP, NewPositions).

solve(Positions, [], Positions).
solve(Positions, Folds, Result) :-
    Folds = [[Dimension, Coord] | Rest],
    fold(Positions, Dimension, Coord, NewPositions),
    solve(NewPositions, Rest, Result).

main :-
    current_prolog_flag(argv, [Filename | _]),
    file_pos_folds(Filename, Positions, Folds),
    Folds = [First | _],
    solve(Positions, [First], FoldedOnce),
    length(FoldedOnce, Result1),
    format('Part 1: ~q~n', [Result1]),
    solve(Positions, Folds, Result2),
    format('Part 2: ~q~n', [Result2]),
    halt.
main :-
    halt(1).
