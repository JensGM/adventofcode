pwds = []
with open('input.txt') as f:
    for line in f.readlines():
        if not line:
            continue
        rng, letter, pwd = line.split()
        letter = letter[0]
        a, b = map(int, rng.split('-'))
        pwds.append({
            'min': a,
            'max': b,
            'letter': letter,
            'pwd': pwd,
        })

# Part one

valid = list(filter(lambda p: p['min'] <= p['pwd'].count(p['letter']) <= p['max'], pwds))
print(len(valid))

# Part two

valid = list(filter(lambda p:
    (p['pwd'][p['min'] - 1] == p['letter']) ^
    (p['pwd'][p['max'] - 1] == p['letter']),
    pwds
))
print(len(valid))
