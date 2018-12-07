from itertools import dropwhile, takewhile
import sys
import re

input = sys.stdin.readlines()

r0 = re.compile(r'^([0-9]+) units each with ([0-9]+) hit points (\(.*\)) with an attack that does ([0-9]+) ([a-z]+) damage at initiative ([0-9]+)$')
r1 = re.compile(r'^(immune|weak) to (.*)$')

class Damage():
    def __init__(self, damage, type_):
        self.damage = int(damage)
        self.type_ = type_

    def __repr__(self):
        return 'Damage(damage={}, type_={})'.format(self.damage, self.type_)

class Combatant:
    def __init__(self, units, hit_points, immunities, weaknesses, damage, initiative):
        self.units = int(units)
        self.hit_points = int(hit_points)
        self.immunities = immunities
        self.weaknesses = weaknesses
        self.damage = damage
        self.initiative = int(initiative)

    def power(self):
        return self.units * self.damage.damage

    def __repr__(self):
        return (
            'Combatant(units={}, hit_points={}, immunities={}, weaknesses={}, '
            'damage={}, initiative={})'
            .format(self.units, self.hit_points, self.immunities,
                    self.weaknesses, self.damage, self.initiative))

class ImmuneSystem(Combatant): pass
class Infection(Combatant): pass

def split_strip(s, delim):
    return list(map(lambda a: a.strip(), s.split(delim)))

def parseln(kls, ln):
    def parseln_internal(ln)
        r = r0.match(ln.strip())
        units = r.group(1)
        hit_points = r.group(2)
        immunities, weaknesses = parseiw(r.group(3))
        damage = Damage(r.group(4), r.group(5))
        initiative = r.group(6)
        return kls(units, hit_points, immunities, weaknesses, damage, initiative)
    return parseln_internal

def parseiw(ln):
    ln = ln.strip('()')
    props = split_strip(ln, ';')
    immunities = []
    weaknesses = []
    for p in props:
        r = r1.match(p)
        t = r.group(1)
        v = split_strip(r.group(2), ',')
        if t == 'immune':
            immunities.extend(v)
        else:
            assert t == 'weak'
            weaknesses.extend(v)
    return immunities, weaknesses

lns = input.copy()
combatants = []

while lns:
    lns = list(dropwhile(lambda a: a.isspace(), lns))
    if not lns: break
    if lns[0].strip() == 'Immune System:':
        combatants.extend(map(parseln(ImmuneSystem), list(takewhile(lambda a: not a.isspace(), lns[1:]))))
    elif lns[0].strip() == 'Infection:':
        combatants.extend(map(parseln(Infection), list(takewhile(lambda a: not a.isspace(), lns[1:]))))
    lns = list(dropwhile(lambda a: not a.isspace(), lns[1:]))

def select_targets(cmbs):
    cmbs = cmbs.copy()
    cmbs.sort(key=lambda a: (a.power(), a.initiative))



while immune_system or infection:
    targets = select_targets(combatants)
    combatants = attack(combatants, targets)
