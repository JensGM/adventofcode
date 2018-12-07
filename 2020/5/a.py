from itertools import tee
from math import ceil
from math import floor
import sys


boarding_passes = [ln.strip() for ln in sys.stdin.readlines() if ln]

def row(boarding_pass, a=0, b=127):
    it = iter(boarding_pass)
    el = next(it)

    if a == b:
        return a
    elif el == 'F':
        return row(it, a=a, b=a + floor((b - a) / 2))
    elif el == 'B':
        return row(it, a=a + ceil((b - a) / 2), b=b)
    else:
        raise RuntimeError('expected resolved range: {}, {}'.format(a, b))

def column(boarding_pass, a=0, b=7):
    if a == b:
        return a

    it = iter(boarding_pass)
    el = next(it)

    if el == 'L':
        return column(it, a=a, b=a + floor((b - a) / 2))
    elif el == 'R':
        return column(it, a=a + ceil((b - a) / 2), b=b)
    else:
        return column(it, a=a, b=b)

def seat_id(row, column):
    return row * 8 + column


resolved = [(row(r), column(c)) for r, c in zip(*tee(boarding_passes))]
seat_ids = sorted(seat_id(r, c) for r, c in resolved)

print(max(seat_ids))
for a, b in zip(seat_ids[0:-1], seat_ids[1:]):
    if b - a == 2:
        print(b - 1)
        break
