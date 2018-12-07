from collections import deque
from functools import reduce
from itertools import chain, takewhile, dropwhile
from operator import or_
import sys


def parse(lines):
    return set((s[5], s[36]) for s in lines if s)

def dot(edges):
    print('digraph g {')
    for e in edges:
        print('\t{} -> {};'.format(e[0], e[1]))
    print('}')

input = parse(sys.stdin.readlines())
V = set(chain(*input))
E = {v: set(b for a, b in input if a == v) for v in V}
Ei = {v: set(a for a, b in input if b == v) for v in V}

sources = set(a for a, adj in Ei.items() if not adj)
visited = []
Q = sorted(list(sources))

while Q:
    v = Q.pop(0)
    if v in visited: continue
    if not Ei[v] <= set(visited):
        continue
    visited.append(v)
    Q.extend(E[v])
    Q.sort()

print(''.join(visited))

work = {v: ord(v) - ord('A') + 62 for v in V}
print(work)
done = set()
in_progress = set()

Q.extend(visited)

t = 0

while Q or in_progress:
    while Q and len(in_progress) < 5:
        v = Q[0]
        if not all(p in done for p in Ei[v]):
            break
        in_progress.add(Q.pop(0))
    for p in in_progress:
        work[p] -= 1
        if work[p] == 0:
            done.add(p)
    in_progress -= done
    t += 1

print(t)
