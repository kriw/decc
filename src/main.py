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
        q, r = abs(int(n)) >> 2, abs(int(n)) & 3
        if reg == 'ebp' and int(n) < 0:
            return 'local_%d_%d' % (q, r)
        elif reg == 'ebp' and int(n) > 0:
            return 'arg_%d_%d' % (q, r)
        else:
            return '*(%s + (%s)' % (reg, n)

def isLiteral(s):
    return re.match(r'\-?[1-9][0-9]*', s) != None or \
            re.match(r'\-?0x[1-9][0-9]*', s) != None

def isNotImplemented(mnem):
    return mnem in ['cdq']

def isFuncMnem(mnem):
    return mnem in ['call', 'push', 'pop', 'leave']

def isAssign(mnem):
    return mnem in ['mov', 'lea', 'add', 'sub', 'imul', 'mul', 'idiv', 'div']

def isLocal(operand):
    return 'local' in operand

def asm2LowIr(insns):
    for i, insn in enumerate(insns):
        if isFuncMnem(insn.mnem):
            #TODO dealing with function call
            insns[i] = Insn(['nop'])
        elif isNotImplemented(insn.mnem):
            insns[i] = Insn(['nop'])
        else:
            insn.ops = list(map(convRef, insn.ops))
            if isAssign(insn.mnem):
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
        
#FIXME rename func name
def isLocalLbl(label, ops):
    return 'local' in label
    # if 'local' in label:
    #     if len(ops) > 0:
    #         return label == ops[0]
    #     elif len(ops) == 0:
    #         return False
    # else:
    #     return False

def isJmpMnem(mnem):
    return mnem in ['jmp', 'jle']
def emit(index, label, insns):
    if isLocalLbl(label, insns[index].ops) or checkDecimal(label):
        return label
    elif insns[index].mnem == 'nop':
        return emit(index-1, label, insns)
    elif isJmpMnem(insns[index].mnem):
        return emit(index-1, label, insns)

    for i in range(index, -1, -1):
        insn = insns[i]
        if isFuncMnem(insn.mnem):
            print("func")
            return "func"
        elif not label in insn.ops:
            continue
        else:
            op, isNumOp = toNumericOp(insn.mnem)
            if insn.mnem == 'mov' or insn.mnem == 'movzx':
                return emit(i-1, insn.ops[1], insns)
            elif insn.mnem == 'idiv' or insn.mnem == 'div':
                return "(%s / %s)" % (emit(i-1, 'eax', insns), emit(i-1, insn.ops[0], insns))
            elif isNumOp:
                return "(%s %s %s)" % (emit(i-1, insn.ops[0], insns), op, emit(i-1, insn.ops[1], insns))
            elif toSetCond(insn.mnem) != None:
                index = i-1
                while insns[index].mnem != 'cmp':
                    index -= 1
                return "(%s %s %s)" % (emit(index-1, insns[index].ops[0], insns), toSetCond(insn.mnem), emit(index-1, insns[index].ops[1], insns))
            else:
                return 'unknow mnemonic'

    return "unknown"

def inputCode(fileName):
    f = open(fileName, 'r')
    insns = list(map(lambda x: x.strip(), f.readlines()))
    return insns

def toJmpCond(mnem):
    if mnem == 'je':
        return "!="
    elif mnem == 'jne':
        return '=='
    elif mnem == 'jle':
        return "<"
    elif mnem == 'jge':
        return ">"
    else:
        return None

def toSetCond(mnem):
    if mnem == 'sete':
        return "=="
    elif mnem == 'setl':
        return "<"
    elif mnem == 'setg':
        return ">"
    else:
        return None

def toNumericOp(mnem):
    if mnem == 'add':
        return "+", True
    elif mnem == 'sub':
        return '-', True
    elif mnem == 'mul' or mnem == 'imul':
        return '*', True
    elif mnem == 'and':
        return '&', True
    elif mnem == 'or':
        return '|', True
    else:
        return 'unknown', False

def isLbl4Jmp(lbl):
    return lbl[0] == '.' and lbl[-1] == ':'

def main(func, argv):
    insns = inputCode(argv[1])
    parsedLines = parse(insns)
    # print('\n'.join(list(map(str, parsedLines))))
    labeledAsm = asm2LowIr(parsedLines)
    print('\n'.join(list(map(str, labeledAsm))))

    print("+++++++++++++++++++++")
    print("%s {" % func)
    for i, l in enumerate(labeledAsm):
        op, isNumOp = toNumericOp(l.mnem)
        if l.mnem == 'mov' and 'local' in l.ops[0]:
            print("%s = %s;" % (l.ops[0], emit(i-1, l.ops[1], labeledAsm)))
        elif isNumOp and 'local' in l.ops[0]:
            left = l.ops[0]
            right1 = emit(i-1, l.ops[0], labeledAsm)
            right2 = emit(i-1, l.ops[1], labeledAsm)
            print("%s = %s %s %s;" % (left, right1, op, right2))
        elif l.mnem == 'ret':
            ret = emit(i-1, 'eax', labeledAsm)
            for j in range(i-1, -1, -1):
                ret = ''
                if labeledAsm[j].mnem == 'mov' and labeledAsm[j].ops[0] == 'eax':
                    ret = emit(j-1, labeledAsm[j].ops[1], labeledAsm)
                    break
            print("return %s;" % ret)
        elif isLbl4Jmp(l.mnem):
            print(l.mnem)
        elif toJmpCond(l.mnem) != None:
            index = i-1
            while labeledAsm[index].mnem != 'cmp':
                index -= 1
            ret = "if(%s %s %s) goto %s;" % (emit(index-1, labeledAsm[index].ops[0], labeledAsm), toJmpCond(l.mnem), emit(index-1, labeledAsm[index].ops[1], labeledAsm), l.ops[0])
            print(ret)
        # else:
        #     print('else: ', l.mnem)
    print("}")


if __name__ == '__main__':
    import sys
    print(sys.argv)
    main('main()', sys.argv)
