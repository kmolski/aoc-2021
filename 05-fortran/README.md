# [Day 5: Hydrothermal Venture](https://adventofcode.com/2021/day/5) solution

NOTE: the data files have been changed from their original representation:
arrows `fx,fy -> tx,ty` converted to commas `fx,fy,tx,ty`

## Expected result
```
$ make run
./a.out example.txt
Part 1:        5
Part 2:       12
./a.out data.txt
Part 1:     6189
Part 2:    19164
$ make test
./a.out example.txt | diff - example.out
./a.out data.txt | diff - data.out
```

## Tested on
```
$ gfortran10 --version
GNU Fortran (FreeBSD Ports Collection) 10.3.0
Copyright (C) 2020 Free Software Foundation, Inc.
This is free software; see the source for copying conditions.  There is NO
warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
```
