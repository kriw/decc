import re

class Node:
    def __init__(self, insn):
        self.label = insn.ops[0]
        initChild(insn)

    def initChild(insn):
        if insn.mnem in ['add', 'sub', 'imul', 'idiv', 'mul', 'div']:
            self.childs = insn.ops
        elif insn.mnem in ['mov', 'lea']:
            self.childs = insn.ops[1:]
        else:
            print('unexepected mnemonic')

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

lblMap = {}
def appendVersion(insn):
    fmt = lambda x, v: x + '.' + str(v)
    if len(insn.ops) > 1:
        lbls = insn.ops[1:]
        for i, l in enumerate(lbls):
            if l in lblMap:
                insn.ops[i+1] = fmt(l, lblMap[l])
                
    lbl = insn.ops[0]
    if not lbl in lblMap:
        lblMap[lbl] = -1
    lblMap[lbl] += 1
    insn.ops[0] = fmt(lbl, lblMap[lbl])
    return insn

def isFuncMnem(mnem):
    return mnem in ['call', 'push', 'pop', 'leave']

def isAssign(mnem):
    return mnem in ['mov', 'add', 'imul']

def isLocal(operand):
    return 'local' in operand

def asm2LowIr(insns):
    for i, insn in enumerate(insns):
        if isFuncMnem(insn.mnem):
            #TODO dealing with function call
            insns[i] = Insn(['nop'])
        else:
            insn.ops = list(map(convRef, insn.ops))
            if isAssign(insn.mnem):
                # insn = appendVersion(insn)
                insns[i] = insn
    return insns
                

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

def checkDecimal(n):
    try:    
        _ = int(n)
        return True
    except ValueError:    
        return False
        
def emit(index, label, insns):
    if 'local' in label or checkDecimal(label):
        return label

    for i in range(index, -1, -1):
        insn = insns[i]
        if isFuncMnem(insn.mnem):
            print("func")
            return "func"
        else:
            if insn.mnem == 'mov':
                return insn.ops[1]
            elif insn.mnem == 'add':
                return "(%s + %s)" % (emit(i-1, insn.ops[0], insns), emit(i-1, insn.ops[1], insns))
            elif insn.mnem == 'imul':
                return "(%s * %s)" % (emit(i-1, insn.ops[0], insns), emit(i-1, insn.ops[1], insns))
            else:
                return 'unknow mnemonic'

    return "unknown"

def inputCode(fileName):
    f = open(fileName, 'r')
    insns = list(map(lambda x: x.strip(), f.readlines()))
    return insns

def main(func, argv):
    insns = inputCode(argv[1])
    parsedLines = parse(insns)
    labeledAsm = asm2LowIr(parsedLines)
    for l in labeledAsm:
        print(str(l))

    print("%s {" % func)
    for i, l in enumerate(labeledAsm):
        if l.mnem == 'mov' and 'local' in l.ops[0]:
            print("%s = %s;" % (l.ops[0], emit(i-1, l.ops[1], labeledAsm)))
        elif l.mnem == 'add' and 'local' in l.ops[0]:
            left = l.ops[0]
            right1 = emit(i-1, l.ops[0], labeledAsm)
            right2 = emit(i-1, l.ops[1], labeledAsm)
            print("%s = %s + %s;" % (left, right1, right2))
        elif l.mnem == 'imul' and 'local' in l.ops[0]:
            left = l.ops[0]
            right1 = emit(i-1, l.ops[0], labeledAsm)
            right2 = emit(i-1, l.ops[1], labeledAsm)
            print("%s = %s * %s;" % (left, right1, right2))
        elif l.mnem == 'ret':
            ret = emit(i-1, 'eax', labeledAsm)
            for j in range(i-1, -1, -1):
                ret = ''
                if labeledAsm[j].mnem == 'mov' and labeledAsm[j].ops[0] == 'eax':
                    ret = emit(j-1, labeledAsm[j].ops[1], labeledAsm)
                    break
            print("return %s;" % ret)
    print("}")


if __name__ == '__main__':
    import sys
    print(sys.argv)
    main('main()', sys.argv)
