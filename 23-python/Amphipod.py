#!/usr/bin/env python3

from copy import deepcopy
from math import inf
from sys import argv

COST_MULTIPLIERS = {"A": 1, "B": 10, "C": 100, "D": 1000}
DESTINATION = {"A": 1, "B": 2, "C": 3, "D": 4}
EXTRA = (["D", "D"], ["C", "B"], ["B", "A"], ["A", "C"])


def parse_burrow(text):
    columns = ["".join(i) for i in zip(*text.splitlines())]
    rooms = [list(columns[x][2:4]) for x in (3, 5, 7, 9)]
    return [None] * 11, *rooms


def make_part2_burrow(burrow):
    corridor, *rooms = burrow
    rooms = [[f, *i, b] for ([f, b], i) in zip(rooms, EXTRA)]
    return corridor, *rooms


def corridor_free(corridor, src, dest):
    if src > dest:
        src, dest = dest, src
    return all(f is None for f in corridor[src : (dest + 1)])


def room_to_room(burrow, depth):
    corridor, room_a, room_b, room_c, room_d = burrow

    def is_legal(src, i, dest):
        return (
            i != dest[src[0]]
            and all(a == src[0] for a in burrow[dest[src[0]]])
            and corridor_free(corridor, i * 2, dest[src[0]] * 2)
        )

    moves = []
    for (i, src) in enumerate((room_a, room_b, room_c, room_d), start=1):
        if src and is_legal(src, i, DESTINATION):
            amphipod = src[0]
            new_state, dest_index = deepcopy(burrow), DESTINATION[amphipod]
            dest = new_state[dest_index]
            src_cost, dest_cost = depth - len(src) + 1, depth - len(dest)
            new_cost = (
                src_cost + abs(i - dest_index) * 2 + dest_cost
            ) * COST_MULTIPLIERS[amphipod]
            new_state[i].pop(0)
            dest.insert(0, amphipod)
            moves.append((new_state, new_cost))

    return moves


def room_to_corridor(burrow, depth):
    corridor, room_a, room_b, room_c, room_d = burrow

    def is_legal(src, i, dest):
        return (
            i != DESTINATION[src[0]] or not all(a == src[0] for a in src)
        ) and corridor_free(corridor, i * 2, dest)

    moves = []
    for (i, src) in enumerate((room_a, room_b, room_c, room_d), start=1):
        for dest in (0, 1, 3, 5, 7, 9, 10):
            if src and is_legal(src, i, dest):
                amphipod = src[0]
                new_state = deepcopy(burrow)
                src_cost = depth - len(src) + 1
                new_cost = (src_cost + abs(i * 2 - dest)) * COST_MULTIPLIERS[amphipod]
                new_state[i].pop(0)
                new_state[0][dest] = amphipod
                dist = abs(dest - DESTINATION[amphipod] * 2)
                moves.append((new_state, new_cost, dist))

    moves.sort(key=lambda it: it[2])
    return [(s, c) for (s, c, _) in moves]


def corridor_to_room(burrow, depth):
    corridor, room_a, room_b, room_c, room_d = burrow

    def is_legal(i, dest):
        offset = 1 if i - 1 <= dest else -1
        return all(a == corridor[i] for a in burrow[dest]) and corridor_free(
            corridor, i + offset, dest * 2
        )

    moves = []
    for i in (0, 1, 3, 5, 7, 9, 10):
        if corridor[i] is not None and is_legal(i, DESTINATION[corridor[i]]):
            amphipod, new_state = corridor[i], deepcopy(burrow)
            dest_i = DESTINATION[amphipod]
            dest = new_state[dest_i]
            dest_cost = depth - len(dest)
            new_cost = (abs(i - dest_i * 2) + dest_cost) * COST_MULTIPLIERS[amphipod]
            new_state[0][i] = None
            dest.insert(0, amphipod)
            moves.append((new_state, new_cost))

    return moves


def next_moves(*args):
    return [*room_to_room(*args), *corridor_to_room(*args), *room_to_corridor(*args)]


def is_organized(burrow):
    corridor, room_a, room_b, room_c, room_d = burrow
    return (
        all(f is None for f in corridor)
        and all(f == "A" for f in room_a)
        and all(f == "B" for f in room_b)
        and all(f == "C" for f in room_c)
        and all(f == "D" for f in room_d)
    )


def solve(burrow, limit, depth, step=0, energy=0, min_cost=inf):
    if is_organized(burrow):
        return energy
    elif step > limit:
        return inf

    for (new_state, cost) in next_moves(burrow, depth):
        if energy + cost > min_cost:
            continue

        total_energy = solve(new_state, limit, depth, step + 1, energy + cost, min_cost)
        if total_energy < min_cost:
            min_cost = total_energy

    return min_cost


with open(argv[1]) as input_file:
    burrow = parse_burrow(input_file.read())

print(f"Part 1: {solve(burrow, 12, 2)}")
print(f"Part 2: {solve(make_part2_burrow(burrow), 28, 4)}")
