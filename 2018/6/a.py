from collections import deque
from sys import stdin


orbits = [ln.strip().split(')') for ln in stdin.readlines() if ln]

class CelestialBody():
    def __init__(self, name, parent=None, satellites=None):
        self.name = name
        self.parent = parent
        self.satellites = satellites if satellites is not None else []

    def __eq__(self, other):
        return hasattr(other, 'name') and other.name == self.name

    def __hash__(self):
        return hash(self.name)

system = {}

for b, s in orbits:
    body = system.setdefault(b, CelestialBody(b))
    satellite = system.setdefault(s, CelestialBody(s))

    body.satellites.append(satellite)
    satellite.parent = body

root = next(body for body in system.values() if body.parent is None)
depth = {root: 0}
queue = deque(sat for sat in root.satellites)

while queue:
    body = queue.popleft()
    if body in depth:
        continue

    depth[body] = depth[body.parent] + 1
    queue.extend(body.satellites)

oribit_count = sum(depth.values())

print(oribit_count)


root = system['YOU'].parent
target = system['SAN'].parent
depth = {root: 0}
next = [root.parent] if root.parent is not None else []
next.extend(sat for sat in root.satellites)
prev = {body: root for body in next}
queue = deque(next)

while queue:
    body = queue.popleft()
    if body is None or body in depth:
        continue

    depth[body] = depth[prev[body]] + 1
    next = [body.parent] if body.parent is not None else []
    next.extend(body.satellites)
    prev.update({n: body for n in next if n not in prev})
    queue.extend(next)

    if body == target:
        break

print(depth[target])
