<?php

function read_data($input_data) {
    list($number_line, $board_data) = explode("\n", $input_data, 2);

    $numbers = array_map('intval', explode(",", $number_line));

    $board_chunks = explode("\n\n", $board_data);
    $boards = [];
    foreach ($board_chunks as $chunk) {
        $board_lines = array_filter(explode("\n", $chunk)); // remove empty lines
        $board_rows = [];
        foreach ($board_lines as $line) {
            $board_rows[] = array_map(
                'intval',
                array_values( // reassign array keys to indices
                    array_filter( // remove empty fields
                        explode(" ", $line),
                        function ($num_str) { return strlen($num_str) > 0; }))
            );
        }
        $boards[] = $board_rows;
    }

    return [$numbers, $boards];
}

function has_full_row(&$board) {
    foreach ($board as &$row) {
        foreach ($row as &$num) {
            if ($num !== -1) { continue 2; } // break to outer for loop
        }
        return true;
    }
    return false;
}

function has_full_col(&$board) {
    for ($c = 0; $c < sizeof($board[0]); ++$c) {
        for ($r = 0; $r < sizeof($board); ++$r) {
            if ($board[$r][$c] !== -1) { continue 2; } // break to outer for loop
        }
        return true;
    }
    return false;
}

function mark_number(&$board, $num) {
    foreach ($board as &$row) {
        foreach ($row as &$v) {
            if ($v === $num) { $v = -1; }
        }
    }
}

function calculate_score(&$board) {
    $sum = 0;
    foreach ($board as &$row) {
        $sum += array_sum(array_filter($row, function ($num) { return $num > 0; }));
    }
    return $sum;
}

function solve(&$numbers, $boards, $break_on_first) {
    $winner_score = null;
    foreach ($numbers as $num) {
        foreach ($boards as &$board) {
            $already_won = has_full_row($board) || has_full_col($board);
            mark_number($board, $num);

            if (!$already_won && (has_full_row($board) || has_full_col($board))) {
                $winner_score = $num * calculate_score($board);
                if ($break_on_first) { return $winner_score; }
            }
        }
    }
    return $winner_score;
}

$input = file_get_contents($argv[1]);

if ($input) {
    list($numbers, $boards) = read_data($input); 

    echo "Part 1: ", solve($numbers, $boards, true), "\n";
    echo "Part 2: ", solve($numbers, $boards, false), "\n";
}

?>
