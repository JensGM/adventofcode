from itertools import product

with open('input.txt') as f:
    input = [int(ln) for ln in f.readlines() if ln]

for a, b, c in product(input, input, input):
    if a + b + c == 2020:
        print(a * b * c)
