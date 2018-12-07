from functools import reduce
from itertools import takewhile
from sys import stdin
print(
    reduce(
        lambda a, b: (a[0] + b[0], a[1] + b[1]),
        map(
            lambda a,
                   f=(lambda b: b // 3 - 2),
                   hack=[]:
                   (
                        hack.clear() or hack.append(a),
                        f(a),
                        sum(takewhile(lambda c: c >= 0, (
                            f(d) for d in hack if not hack.append(f(d))
                        ))),
                   )[1:],
            map(
                int,
                filter(bool, stdin.readlines())
            )
        )
    )
)
