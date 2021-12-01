<?php

function read_ints_from_file($file) {
    $ints = [];
    while (!feof($file)) {
        $ints[] = intval(fgets($file));
    }
    fclose($file);
    return $ints;
}

function get_stack_sum($stack) {
    $sum = 0;
    for ($i = 0; $i < sizeof($stack); ++$i) {
        $sum += $stack[$i];
    }
    return $sum;
}

function solve($ints, $window_size) {
    $counter = 0;
    $stack = [];

    for ($i = 0; $i < $window_size; ++$i) {
        $stack[] = $ints[$i];
    }

    $prev_sum = get_stack_sum($stack);
    for ($i = $window_size; $i < sizeof($ints); ++$i) {
        array_shift($stack);
        $stack[] = $ints[$i];

        $last_sum = get_stack_sum($stack);
        if ($last_sum > $prev_sum) {
            ++$counter;
        }

        $prev_sum = $last_sum;
    }

    return $counter;
}

$input_file = fopen($argv[1], "r");

if ($input_file) {
    $ints = read_ints_from_file($input_file);

    echo "Part 1: ", solve($ints, 1), "\n";
    echo "Part 2: ", solve($ints, 3), "\n";
}

?>
