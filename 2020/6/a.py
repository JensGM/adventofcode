from functools import reduce
from itertools import groupby
import sys


answers = map(str.strip, sys.stdin.readlines())
answers = [list(group) for k, group in groupby(answers, lambda ln: ln != '') if k]

anyone = [''.join(lns) for lns in answers]
anyone = [set(a) for a in anyone]
print(sum(len(s) for s in anyone))

everyone = [reduce(set.intersection, s) for s in map(lambda g: map(set, g), answers)]
print(sum(len(s) for s in everyone))
