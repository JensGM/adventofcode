from dataclasses import dataclass, fields
from itertools import groupby
import re
import sys


@dataclass
class Passport:
    byr: str = None
    iyr: str = None
    eyr: str = None
    hgt: str = None
    hcl: str = None
    ecl: str = None
    pid: str = None
    cid: str = None

    def valid(self, constraints=[]) -> bool:
        for i, constraint in enumerate(constraints):
            try:
                if not constraint(self):
                    print('constraint', i, 'failed')
                    return False
            except:
                print('constraint', i, 'mega failed')
                return False
        return True


passports = map(str.strip, sys.stdin.readlines())
passports = [list(group) for k, group in groupby(passports, lambda ln: ln != '') if k]
passports = [' '.join(lns).split() for lns in passports]
passports = [{key: value for key, value in map(lambda f: f.split(':'), d)} for d in passports]
passports = [Passport(**p) for p in passports]

constraints = [
    lambda i: all(getattr(i, f.name) is not None for f in fields(i) if f.name != 'cid')
]
print(len([p for p in passports if p.valid(constraints)]))


hgt_pattern = re.compile(r'^([0-9]+)(cm|in)$')
hcl_pattern = re.compile(r'^#[a-f0-9]{6}$')
pid_pattern = re.compile(r'^[0-9]{9}$')
constraints = [
    lambda i: all(getattr(i, f.name) is not None for f in fields(i) if f.name != 'cid'),
    lambda i: 1920 <= int(i.byr) <= 2002,
    lambda i: 2010 <= int(i.iyr) <= 2020,
    lambda i: 2020 <= int(i.eyr) <= 2030,
    lambda i: hgt_pattern.match(i.hgt) and 150 <= int(i.hgt[:-2]) <= 193 if i.hgt[-2:] == 'cm' else 59 <= int(i.hgt[:-2]) <= 76,
    lambda i: hcl_pattern.match(i.hcl),
    lambda i: i.ecl in {'amb', 'blu', 'brn', 'gry', 'grn', 'hzl', 'oth'},
    lambda i: pid_pattern.match(i.pid),
]
print(len([p for p in passports if p.valid(constraints)]))

