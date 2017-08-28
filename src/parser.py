import insn
import re
import copy

def inputCode(fileName):
    f = open(fileName, 'r')
    insns = list(map(lambda x: x.strip(), f.readlines()))
    return insns

def parse(insns):
    parsedLines = []
    for _insn in insns:
        a = _insn.split('\t')
        if len(a) > 1:
            b = a[1].split(',')
            c = list(map(lambda x: x.strip(), [a[0]] + b))
        else:
            c = a
        parsedLines.append(insn.Insn(c))
    return parsedLines

def parseObjdump(lines):
    addrs = []
    insns = []
    for i in range(len(lines)):
        addr, _, ins = lines[i].strip().split('\t')
        addrs.append(addr[:-1])
        insns.append(ins)
    lines = list(map(lambda x: ' '.join(x.strip().split('\t')), lines))

    _insns = copy.deepcopy(insns)
    labelAddr = set()
    for i, ins in enumerate(_insns):
        tmp = ins.split()
        mnem, ops = tmp[0], tmp[1:]
        if insn.isJmpMnem(mnem) or mnem == 'call':
            insns[i] = '%s\t%s' % (mnem, ops[0])
            labelAddr.add(ops[0])
        else:
            insns[i] = '%s\t%s' % (mnem, ' '.join(ops))

    def _genLabel():
        n = -1
        mem = {}
        def get(addr):
            nonlocal n
            n += 1
            if not addr in mem:
                mem[addr] = 'L%d' % n
            return mem[addr]
        return get
    genLabel = _genLabel()

    
    _insns = copy.deepcopy(insns)
    insertList = []
    for i, (addr, ins) in enumerate(zip(addrs, _insns)):
        tmp = ins.split()
        if len(tmp) <= 1:
            continue
        ops = tmp[1:]
        if ops[0] in labelAddr:
            label = genLabel(ops[0])
            insns[i] = '%s\t%s' % (str(insns[i].split()[0]), label)
            if ops[0] in addrs:
                insertList.append((addrs.index(ops[0]), label))

    for index, label in sorted(insertList, reverse=True):
        insns.insert(index, '%s:' % label)
    return insns
