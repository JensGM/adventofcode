import sys


program = [inst.strip().split() for inst in sys.stdin if inst.strip()]
program = [(op, int(val)) for op, val in program]

instpointer = 0
accumulator = 0

executed = set()

while instpointer not in executed:
    executed.add(instpointer)
    op, val = program[instpointer]
    
    if op == 'nop':
        pass
    elif op == 'acc':
        accumulator += val
    elif op == 'jmp':
        instpointer += val
        continue
    else:
        raise RuntimeError

    instpointer += 1


print(accumulator)


bug = instpointer - 1

instpointer = 0
accumulator = 0

executed = set()


while instpointer not in executed:
    executed.add(instpointer)
    op, val = program[instpointer]
    
    if op == 'nop':
        pass
    elif op == 'acc':
        accumulator += val
    elif op == 'jmp':
        instpointer += val
        continue
    else:
        raise RuntimeError

    instpointer += 1

print(accumulator)
