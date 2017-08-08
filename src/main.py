import re

class Insn:
    def __init__(self, insn):
        self.mnem = insn[0]
        self.ops = insn[1:]
    def __str__(self):
        return str(self.mnem) + "\t" \
                + ', '.join(self.ops)

def convRef(s):
    replacedStr =  re.sub(
            r'DWORD PTR ([\-][1-9][0-9]*)\[(ebp)\]',
            r'\2,\1', s)
    if replacedStr == s:
        return s
    else:
        reg, n = replacedStr.split(',')
        q, r = abs(int(n)) >> 4, abs(int(n)) & 3
        if reg == 'ebp' and int(n) < 0:
            return 'local_%d_%d' % (q, r)
        elif reg == 'ebp' and int(n) > 0:
            return 'arg_%d_%d' % (q, r)
        else:
            return '*(%s + (%s)' % (reg, n)

REGS = ['eax', 'ebx', 'ecx', 'edx', 'edi', 'esi']

def isLiteral(s):
    return re.match(r'\-?[1-9][0-9]*', s) != None or \
            re.match(r'\-?0x[1-9][0-9]*', s) != None

def toCode(insns):
    if not 'local' in insns[0].ops[0]:
        print(":Ha:")

    for insn in insns:
        if insn.mnem in ['mov', 'lea']:
            if 'local' in insn.ops[0]:
                if isLiteral(insn.ops[1]):
                    #end


def asm2LowIr(insns):
    for i, insn in enumerate(insns):
        if insn.mnem in ['call', 'push', 'pop', 'leave']:
            print("=====")
            
        else:
            insn.ops = list(map(convRef, insn.ops))
            if insn.mnem in ['mov', 'lea'] and 'local' in insn.ops[0]:
                toCode(insns[:i+1][::-1])
                

def parse(insns):
    parsedLines = []
    for insn in insns:
        a = insn.split('\t')
        if len(a) > 1:
            b = a[1].split(',')
            c = list(map(lambda x: x.strip(), [a[0]] + b))
        else:
            c = a
        parsedLines.append(Insn(c))
    return parsedLines

def inputCode(fileName):
    f = open(fileName, 'r')
    insns = list(map(lambda x: x.strip(), f.readlines()))
    return insns

def main(argv):
    insns = inputCode(argv[1])
    parsedLines = parse(insns)
    asm2LowIr(parsedLines)


if __name__ == '__main__':
    import sys
    print(sys.argv)
    main(sys.argv)
