# [Day 16: Packet Decoder](https://adventofcode.com/2021/day/16) solution

NOTE: the parsing subroutine uses two Fortran 90 features: array slicing and recursive subroutines.
Even though it is possible to emulate their behavior by adding arguments for the slice bounds and
(exploiting compiler implementation details)[https://sites.esm.psu.edu/~ajm138/fortranexamples.html#ex1],
this would have made the already lengthy code even longer (and less readable).

To avoid recursion in the solution for part 2 (packet evaluation), we can use a property of the
parsing algorithm - subpackets are always pushed onto the arrays after their parent packet.

So, instead of calling `EVAL` on each subpacket and then performing the operation, the subpackets
can be evaluated last-to-first, pushing their computed values to the parent packet if its value is empty,
or updating the present value with the operation and subpacket value.

To facilitate this, the `PARSE` subroutine computes the values for literal packets immediately, while the
values for operator packets are initialized to `-1` (empty/not computed yet).

## Expected result
```
$ make run
./a.out example.txt; ./a.out data.txt
Part 1:           19
Part 2:            0
Part 1:          879
Part 2: 539051801941
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
