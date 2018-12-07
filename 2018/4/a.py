from functools import reduce
from sys import argv

start, end = int(argv[1]), int(argv[2])

valid = 0

for i in range(start, end + 1):
    digits = str(i)
    pairs = list(zip(digits[:-1], digits[1:]))
    if (len(digits) == 6 and
        all(d0 <= d1 for d0, d1 in pairs) and
        any(d0 == d1 for d0, d1 in pairs) and
            (
                (digits[0] == digits[1] and digits[1] != digits[2]) or
                (digits[0] != digits[1] and digits[1] == digits[2] and digits[2] != digits[3]) or
                (digits[1] != digits[2] and digits[2] == digits[3] and digits[3] != digits[4]) or
                (digits[2] != digits[3] and digits[3] == digits[4] and digits[4] != digits[5]) or
                (digits[3] != digits[4] and digits[4] == digits[5])
            )
        ):
        valid += 1

print(valid)
