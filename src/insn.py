import re

class Insn:
    def __init__(self, insn):
        self.mnem = insn[0]
        self.ops = insn[1:]
    def __str__(self):
        return str(self.mnem) + "\t" \
                + ', '.join(self.ops)

#FIXME rename func name
def isLocalLbl(label, ops):
    return 'local' in label

def isJmpMnem(mnem):
    return mnem in ['jmp', 'je', 'jne', 'jge', 'jg', 'jle', 'jl']

def isFuncMnem(mnem):
    return mnem in ['call', 'push', 'pop', 'leave']

def isNotImplemented(mnem):
    return mnem in ['cdq']

def isAssign(mnem):
    return mnem in ['mov', 'lea', 'add', 'sub', 'imul', 'mul', 'idiv', 'div']

def isLocal(operand):
    return 'local' in operand

def isLiteral(s):
    return re.match(r'\-?[0x][1-9][0-9]*', s) != None
       
def convRef(s):
    replacedStr =  re.sub(
            r'DWORD PTR \[(ebp)([\-\+]0x[1-9a-fA-F][0-9a-fA-F]*)\]',
            r'\1,\2', s)
    if replacedStr == s:
        return s
    else:
        reg, n = replacedStr.split(',')
        q, r = abs(int(n, 16)) >> 2, abs(int(n, 16)) & 3
        if reg == 'ebp' and int(n, 16) < 0:
            return 'local_%d_%d' % (q, r)
        elif reg == 'ebp' and int(n, 16) > 0:
            return 'arg_%d_%d' % (q, r)
        else:
            return '*(%s + (%s)' % (reg, n)

def asm2LowIr(insns):
    for i, insn in enumerate(insns):
        if isFuncMnem(insn.mnem):
            #TODO dealing with function call
            insn.ops = list(map(convRef, insn.ops))
        elif isNotImplemented(insn.mnem):
            insns[i] = Insn(['nop'])
        else:
            insn.ops = list(map(convRef, insn.ops))
            if isAssign(insn.mnem):
                insns[i] = insn
    return insns
         
