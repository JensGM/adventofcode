from enum import IntEnum
from sys import argv


class Instruction(IntEnum):
    ADD = 1
    MULT = 2
    INPUT = 3
    OUTPUT = 4
    JIT = 5
    JIF = 6
    LT = 7
    EQ = 8
    EXIT = 99


class Mode(IntEnum):
    POSITION = 0
    IMIDIATE = 1


def user_input():
    while True:
        try:
            inp = input()
            val = int(inp)
            return val
        except ValueError:
            print('Invalid input \'{}\''.format(inp))


def interp_inst(code):
    code = '{:05d}'.format(code)
    mode = list(reversed([int(c) for c in code[:3]]))
    inst = int(code[3:])
    return mode, inst


def load(prog, ip, offset, mode):
    addr = ip + offset
    if mode[offset - 1] == Mode.POSITION:
        addr = prog[addr]
    return prog[addr]


with open(argv[1]) as f:
    prog = [int(s) for s in f.read().split(',') if s]


ip = 0
while ip < len(prog):
    mode, inst = interp_inst(prog[ip])
    # print(ip, inst, prog)

    if inst == Instruction.EXIT:
        break

    elif inst == Instruction.ADD:
        a = load(prog, ip, 1, mode)
        b = load(prog, ip, 2, mode)
        addr = prog[ip + 3]
        prog[addr] = a + b
        ip += 4

    elif inst == Instruction.MULT:
        a = load(prog, ip, 1, mode)
        b = load(prog, ip, 2, mode)
        addr = prog[ip + 3]
        prog[addr] = a * b
        ip += 4

    elif inst == Instruction.INPUT:
        addr = prog[ip + 1]
        prog[addr] = user_input()
        ip += 2

    elif inst == Instruction.OUTPUT:
        addr = prog[ip + 1]
        print(prog[addr])
        ip += 2

    elif inst == Instruction.JIT:
        condition = load(prog, ip, 1, mode)
        ptr = load(prog, ip, 2, mode)
        ip = ptr if condition != 0 else ip + 3

    elif inst == Instruction.JIF:
        condition = load(prog, ip, 1, mode)
        ptr = load(prog, ip, 2, mode)
        ip = ptr if condition == 0 else ip + 3

    elif inst == Instruction.LT:
        a = load(prog, ip, 1, mode)
        b = load(prog, ip, 2, mode)
        addr = prog[ip + 3]
        prog[addr] = 1 if a < b else 0
        ip += 4

    elif inst == Instruction.EQ:
        a = load(prog, ip, 1, mode)
        b = load(prog, ip, 2, mode)
        addr = prog[ip + 3]
        prog[addr] = 1 if a == b else 0
        ip += 4


    else:
        ip += 1
