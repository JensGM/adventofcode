#!/usr/bin/env python3

from string import ascii_lowercase, ascii_uppercase
import colorama
import numpy as np
import sys

colorama.init()

names = ascii_lowercase + ascii_uppercase

lines = sys.stdin.readlines()

def parse(s):
    s = ''.join(s.split())
    return tuple(map(int, s.split(',')))

def style(s, n, bright):
    bcolors = [
        colorama.Back.RED, colorama.Back.GREEN, colorama.Back.YELLOW,
        colorama.Back.BLUE, colorama.Back.MAGENTA, colorama.Back.CYAN,
        colorama.Back.WHITE,
    ]
    fcolors = [
        colorama.Fore.RED, colorama.Fore.GREEN, colorama.Fore.YELLOW,
        colorama.Fore.BLUE, colorama.Fore.MAGENTA, colorama.Fore.CYAN,
        colorama.Fore.WHITE,
    ]
    return (
        bcolors[n % len(bcolors)]
        + (colorama.Fore.BLACK if bright else fcolors[n % len(fcolors)])
        + (colorama.Style.BRIGHT if not bright else colorama.Style.DIM)
        + s
        + colorama.Style.RESET_ALL)

def display(M, pts):
    M = M.T
    for i, m in enumerate(M):
        print(''.join(style(names[n], n, (k, i) in pts)
            if n >= 0 else ' ' for k, n in enumerate(m)), '§')

def _circle(x, y, r):
    return ([]
        + [(x - r + i, y + i) for i in range(r)]
        + [(x + i, y + r - i) for i in range(r)]
        + [(x + r - i, y - i) for i in range(r)]
        + [(x - i, y - r + i) for i in range(r)]
    )

pts = list(filter(bool, [parse(line) for line in lines]))
# pts.sort(key=lambda x: (x[0], x[1]))
npts = len(pts)

u0, v0 = min(pts, key=lambda p: p[0])[0], min(pts, key=lambda p: p[1])[1]
u1, v1 = max(pts, key=lambda p: p[0])[0], max(pts, key=lambda p: p[1])[1]
x0, y0 = 0, 0
x1, y1 = u1 - u0, v1 - v0

pts = [(x - u0, y - v0) for x, y in pts]

grid = np.ndarray((x1 + 1, y1 + 1), np.int32)
grid[:] = -1

dist = np.ndarray((x1 + 1, y1 + 1), np.int32)
dist[:] = -1

sizes = np.ndarray((npts,), np.int32)
sizes[:] = 0

for i, (x, y) in enumerate(pts):
    dist[x,y] = 0
    grid[x,y] = i
    sizes[i] = 1

radius = 1
iternr = 0
while True:
    ops = 0
    for i, (x, y) in enumerate(pts):
        circle = [(u, v) for u, v in _circle(x, y, radius)
            if 0 <= u <= x1 and 0 <= v <= y1]
        for u, v in circle:
            if grid[u,v] == -1:
                grid[u,v] = i
                dist[u,v] = radius
                sizes[i] += 1
                ops += 1
            elif grid[u,v] != -2:
                if dist[u,v] == radius:
                    sizes[grid[u,v]] -= 1
                    grid[u,v] = -2
                    ops += 1

    iternr += 1
    if iternr % 20 == 0:
        display(grid, pts)
    if ops == 0:
        break

    radius += 1

display(grid, pts)

edge = set()
for i in range(x1 + 1):
    if grid[i,0] >= 0: edge.add(grid[i,0])
    if grid[i,-1] >= 0: edge.add(grid[i,-1])
for i in range(y1 + 1):
    if grid[0,i] >= 0: edge.add(grid[0,i])
    if grid[-1,i] >= 0: edge.add(grid[-1,i])

print('edge:', [names[e] for e in edge])
print(max([s for i, s in enumerate(sizes) if i not in edge]))

def mdist(x, y, u, v):
    return abs(x - u) + abs(y - v)

mask_size = 0
for x in range(-200, x1 + 200):
    for y in range(-200, y1 + 200):
        total_dist = 0
        for u, v in pts:
            total_dist += mdist(x, y, u, v)
        if total_dist < 10000:
            mask_size += 1
print(mask_size)
