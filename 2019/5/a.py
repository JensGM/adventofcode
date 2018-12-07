#!/usr/bin/env python3

from string import ascii_lowercase
from itertools import zip_longest as zipl
import sys

def p(x): return x.isupper()
def t(x): return x.lower()
polymer = tuple(sys.stdin.read().strip())
u = sys.argv[1]

plmr = tuple(a for a in polymer if a.lower() != u)
length = len(plmr)
while True:
    z = reversed(list(enumerate(zip(plmr[1:], plmr[:-1]))))
    plmr = sum((
        (a,) if i else (a, b) for i, (a, b) in z
        if t(a) != t(b) or p(a) == p(b) or i == 0 or (next(z) and False)
    ), tuple())
    if len(plmr) == length:
        break
    length = len(plmr)
print(u, ':', length)
