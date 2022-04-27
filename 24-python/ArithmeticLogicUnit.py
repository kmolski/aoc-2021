#!/usr/bin/env python3

from sys import argv

from z3 import And, If, Optimize, IntVector, FreshInt, Sum, sat


def solve(program, opt):
    solver = Optimize()
    z = 0

    serial = IntVector("serial", 14)
    solver.add([And(1 <= d, d <= 9) for d in serial])

    for i, batch in ((n, program[n * 18 : (n + 1) * 18]) for n in range(14)):
        param1, param2, param3 = (int(batch[n][2]) for n in (4, 5, 15))
        w = serial[i]
        x = z % 26
        z /= param1
        z = If(w != x + param2, z * 26 + w + param3, z)

    solver.add(z == 0)

    serial_value = FreshInt()
    solver.add(serial_value == Sum([d * 10 ** (13 - i) for i, d in enumerate(serial)]))
    result = opt(solver, serial_value)

    if solver.check() == sat:
        return result.value()
    raise Exception("UNSAT: " + str(solver.model()))


with open(argv[1]) as input_file:
    program = [line.split() for line in input_file.readlines()]

print(f"Part 1: {solve(program, Optimize.maximize)}")
print(f"Part 2: {solve(program, Optimize.minimize)}")
