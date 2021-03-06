# [Day 13: Transparent Origami](https://adventofcode.com/2021/day/13) solution

NOTE: The solution for part 2 is "PZFJHRFZ", as shown in the `letters.txt` file.

## Expected result
```
$ make run
swipl TransparentOrigami.pl example.txt; swipl TransparentOrigami.pl data.txt
Part 1: 17
Part 2: [[0,0],[0,1],[0,2],[0,3],[0,4],[1,0],[1,4],[2,0],[2,4],[3,0],[3,4],[4,0],[4,1],[4,2],[4,3],[4,4]]
Part 1: 610
Part 2: [[0,0],[0,1],[0,2],[0,3],[0,4],[0,5],[1,0],[1,3],[2,0],[2,3],[3,1],[3,2],[5,0],[5,4],[5,5],[6,0],[6,3],[6,5],[7,0],[7,2],[7,5],[8,0],[8,1],[8,5],[10,0],[10,1],[10,2],[10,3],[10,4],[10,5],[11,0],[11,2],[12,0],[12,2],[13,0],[15,4],[16,5],[17,0],[17,5],[18,0],[18,1],[18,2],[18,3],[18,4],[20,0],[20,1],[20,2],[20,3],[20,4],[20,5],[21,2],[22,2],[23,0],[23,1],[23,2],[23,3],[23,4],[23,5],[25,0],[25,1],[25,2],[25,3],[25,4],[25,5],[26,0],[26,3],[27,0],[27,3],[27,4],[28,1],[28,2],[28,5],[30,0],[30,1],[30,2],[30,3],[30,4],[30,5],[31,0],[31,2],[32,0],[32,2],[33,0],[35,0],[35,4],[35,5],[36,0],[36,3],[36,5],[37,0],[37,2],[37,5],[38,0],[38,1],[38,5]]
$ make test
swipl TransparentOrigami.pl example.txt | diff - example.out
swipl TransparentOrigami.pl data.txt | diff - data.out
```

## Tested on
```
$ swipl --version
SWI-Prolog version 8.2.3 for amd64-freebsd
```
