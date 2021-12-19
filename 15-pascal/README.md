# [Day 15: Chiton](https://adventofcode.com/2021/day/15) solution

NOTE: the solution may take 10 minutes (or longer) to complete on the full input data.

## Expected result
```
$ make run
./Chiton example.txt; ./Chiton data.txt
Part 1: 
Visited 98 out of 100 nodes
40
Part 2: 
Visited 2498 out of 2500 nodes
315
Part 1: 
Visited 9998 out of 10000 nodes
562
Part 2: 
Visited 249998 out of 250000 nodes
2874
$ make test
./Chiton example.txt | diff - example.out
./Chiton data.txt | diff - data.out
```

## Tested on
```
$ fpc -i
Free Pascal Compiler version 3.2.2

Compiler date      : 2021/10/05
Compiler CPU target: x86_64
...
```

## Resources
* [Dijkstra's algorithm](https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm)
