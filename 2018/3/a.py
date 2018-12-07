from functools import reduce
from itertools import chain, repeat
from sys import stdin

wires = [ln.split(',') for ln in stdin.readlines() if ln]

def add(a, b):
    return a[0] + b[0], a[1] + b[1]

def manhattan_distance(a, b):
    return abs(b[0] - a[0]) + abs(b[1] - a[1])

def trace(pnt, path, distance=None, distances=None):
    if len(path) == 0:
        return frozenset([]), distances

    if distance is None:
        distance = 0
    if distances is None:
        distances = {}

    x, *xs = path
    dir_name, step_count = x[0], int(x[1:])

    if   dir_name == 'U': dir = ( 0,  1)
    elif dir_name == 'R': dir = ( 1,  0)
    elif dir_name == 'D': dir = ( 0, -1)
    elif dir_name == 'L': dir = (-1,  0)

    steps = [pnt]
    if pnt not in distances:
        distances[pnt] = distance

    for step in range(step_count):
        prev = steps[-1]
        this = add(prev, dir)
        steps.append(this)
        if this not in distances:
            distances[this] = distance + step + 1

    future, distances = trace(steps[-1], xs, distance + step_count, distances)

    return frozenset(steps) | future, distances

o = (0, 0)
p0, d0 = trace(o, wires[0])
p1, d1 = trace(o, wires[1])

intersections = (p0 & p1) - frozenset([o])
closest = min(intersections, key=lambda p: manhattan_distance(o, p))
fastest = min(intersections, key=lambda p: d0[p] + d1[p])

print(manhattan_distance(o, closest))
print(d0[fastest] + d1[fastest])
