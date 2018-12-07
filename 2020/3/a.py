from itertools import count
from functools import reduce
import colorama


with open('input', 'r') as f:
    lns = [list(ln.strip()) for ln in f.readlines() if ln]
column_count = len(lns[0])

slope = 3, 1



def hit_trees(slope):
    return len([
        (i, k) for i, k in
        zip(
            count(0, slope[0]), # x
            range(0, len(lns), slope[1]) # y
        )
        if lns[k][i % column_count] == '#'
    ])

print(reduce(lambda a, b: a * b, map(hit_trees, ((1, 1), (3, 1), (5, 1), (7, 1), (1, 2)))))
