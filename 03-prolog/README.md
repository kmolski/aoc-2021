# [Day 3: Binary Diagnostic](https://adventofcode.com/2021/day/3) solution

## Expected result
```
$ make run
swipl BinaryDiagnostic.pl example.txt
Part 1: 198
Part 2: 230
swipl BinaryDiagnostic.pl data.txt
Part 1: 4191876
Part 2: 3414905
$ make test
swipl BinaryDiagnostic.pl example.txt | diff - example.out
swipl BinaryDiagnostic.pl data.txt | diff - data.out
```

## Tested on
```
$ swipl --version
SWI-Prolog version 8.2.3 for amd64-freebsd
```
