from itertools import product, takewhile
from sys import stdin

print('\n'.join(map(lambda result: 'result: {}, input: {}'.format(
                result[4], 100 * result[1] + result[2]), list(
        (lambda source=list(map(int, filter(bool, stdin.read().split(',')))):
            filter(lambda candidate:
                    candidate[4] == 19690720 or
                    (candidate[1] == 12 and candidate[2] == 2),
                map(lambda vals, prog=source[:]: (
                    prog.clear() or prog.extend(source),
                    vals[0], vals[1],
                    list(takewhile(
                        lambda opcode: opcode != 99,
                        (
                            prog[inst] for inst in range(-4, len(prog), 4)
                            if (inst == -4 and (prog.__setitem__(1, vals[0]) or
                                                prog.__setitem__(2, vals[1]))
                                           or inst == -4)
                            or (prog[inst] == 1 and not prog.__setitem__(
                               prog[inst + 3], prog[prog[inst + 1]]
                                             + prog[prog[inst + 2]]))
                            or (prog[inst] == 2 and not prog.__setitem__(
                               prog[inst + 3], prog[prog[inst + 1]]
                                             * prog[prog[inst + 2]]))
                            or (prog[inst] == 99))
                        )
                    ),
                    prog[0]
                ),
                product(range(100), range(100))
            )
        )
    )()
))))
