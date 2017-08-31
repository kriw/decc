import re
import insn
import parser
import parseObjdump

def checkDecimal(n, hexadecimal=True):
    try:    
        _ = int(n, 16) if hexadecimal else int(n)
        return True
    except ValueError:    
        return False
        
def checkRetValUsed(index, insns):
    for ins in insns[index:]:
        if len(ins.ops) > 1 and ins.ops[1] == 'eax' and insn.isAssign(ins.mnem):
            return True
        elif ins.mnem in ['call']:
            return False
        elif len(ins.ops) > 0 and ins.ops[0] == 'eax' and insn.isAssign(ins.mnem):
            return False
    return False

def restructureFunc(index, funcName, insns):
    args = []
    while insns[index].mnem == 'push':
        args.append(insns[index].ops[0])
        index -= 1
    return "%s(%s)" % (funcName, ', '.join(args))

def emit(index, label, insns):
    if insn.isVariableLbl(label, insns[index].ops) or checkDecimal(label):
        return label
    elif insns[index].mnem == 'nop':
        return emit(index-1, label, insns)
    elif insn.isJmpMnem(insns[index].mnem):
        return emit(index-1, label, insns)

    for i in range(index, -1, -1):
        ins = insns[i]
        if insn.isFuncMnem(ins.mnem):
            if ins.mnem == 'call' and checkRetValUsed(i+1, insns):
                return restructureFunc(i-1, ins.ops[0], insns)
            else:
                continue
        elif not label in ins.ops:
            continue
        else:
            op, isNumOp = toNumericOp(ins.mnem)
            if ins.mnem == 'mov' or ins.mnem == 'movzx':
                return emit(i-1, ins.ops[1], insns)
            elif ins.mnem == 'idiv' or ins.mnem == 'div':
                return "(%s / %s)" % (emit(i-1, 'eax', insns), emit(i-1, ins.ops[0], insns))
            elif isNumOp:
                return "(%s %s %s)" % (emit(i-1, ins.ops[0], insns), op, emit(i-1, ins.ops[1], insns))
            elif toSetCond(ins.mnem) != None:
                index = i-1
                while insns[index].mnem != 'cmp':
                    index -= 1
                return "(%s %s %s)" % (emit(index-1, insns[index].ops[0], insns), toSetCond(ins.mnem), emit(index-1, insns[index].ops[1], insns))
            else:
                return None

    return None


def toJmpCond(mnem):
    if mnem == 'je':
        return "!="
    elif mnem == 'jne':
        return '=='
    elif mnem == 'jle':
        return "<="
    elif mnem == 'jl':
        return '<'
    elif mnem == 'jge':
        return ">="
    elif mnem == 'jg':
        return '>'
    elif mnem == 'jmp':
        return 'jmp'
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
    return lbl[-1] == ':'

def findLbl(label, insns):
    for i, ins in enumerate(insns):
        if ins.mnem == '%s:' % label:
            return i
    return None

def findLblFromCode(label, code):
    for i, line in enumerate(code):
        if line == '%s:' % label:
            return i
    return None

def decLabeledAsm(func, labeledAsm):
    result = []
    write = lambda x: result.append(x)
    labelStack = [('', '')]

    write("%s {" % func)
    for i, l in enumerate(labeledAsm):
        op, isNumOp = toNumericOp(l.mnem)
        if l.mnem == 'mov' and 'local' in l.ops[0]:
            write("%s = %s;" % (l.ops[0], emit(i-1, l.ops[1], labeledAsm)))
        elif isNumOp and 'local' in l.ops[0]:
            left = l.ops[0]
            right1 = emit(i-1, l.ops[0], labeledAsm)
            right2 = emit(i-1, l.ops[1], labeledAsm)
            write("%s = %s %s %s;" % (left, right1, op, right2))
        elif l.mnem == 'ret':
            ret = emit(i-1, 'eax', labeledAsm)
            if ret == None:
                for j in range(i-1, -1, -1):
                    ret = ''
                    if labeledAsm[j].mnem == 'mov' and labeledAsm[j].ops[0] == 'eax':
                        ret = emit(j-1, labeledAsm[j].ops[1], labeledAsm)
                        break
            write("return %s;" % ret)
        elif isLbl4Jmp(l.mnem):
            flg = True
            for i, (lbl, s) in enumerate(labelStack[::-1]):
                if l.mnem == lbl:
                    flg = False
                    write(s)
                    labelStack = labelStack[:i] + labelStack[i+1:]
            write(l.mnem)
        elif toJmpCond(l.mnem) != None:
            operator = toJmpCond(l.mnem)
            label = l.ops[0]
            lblIndex = findLbl(label, labeledAsm)
            ret = ''

            index = i-1
            while index >= 0 and labeledAsm[index].mnem != 'cmp':
                index -= 1
            if index >= 0:
                ret = "if(%s %s %s) goto %s;" % (emit(index-1, labeledAsm[index].ops[0], labeledAsm), toJmpCond(l.mnem), emit(index-1, labeledAsm[index].ops[1], labeledAsm), l.ops[0])
                labelStack.append((label ,'}'))
            elif operator == 'jmp':
                ret = 'goto %s;' % l.ops[0]
            write(ret)
        elif l.mnem == 'call' and not checkRetValUsed(i+1, labeledAsm):
            ret = restructureFunc(i-1, l.ops[0], labeledAsm)
            write(ret + ';')
    write("}")
    return result

def getLabelFromIf(line):
    patternIf = r'(if\(.*\)) goto (.*);'
    label = re.sub(patternIf, r'\2', line);
    statement = re.sub(patternIf, r'\1 {', line)
    return label, statement

def getLabelFromGoto(line):
    label = re.sub(r'goto (.*);', r'\1', line);
    return label

def ctlSt(codeWithGoto):
    #if statement
    replaceLabels = []
    for i, line in enumerate(codeWithGoto):
        lbl, statement = getLabelFromIf(line)
        if lbl == None or statement == line:
            continue
        lblIndex = findLblFromCode(lbl, codeWithGoto)
        if lblIndex == None or i >= lblIndex:
            continue
        codeWithGoto[i] = statement
        replaceLabels.append(lblIndex)

    for i in replaceLabels:
        if '}' in codeWithGoto[i]:
            codeWithGoto[i] += '}'
        else:
            codeWithGoto[i] = '}'

    #for statement
    replaceLabels = []
    replaceList = []
    for i, line in enumerate(codeWithGoto):
        lbl, statement = getLabelFromIf(line)
        if lbl == None or statement == None:
            continue
        lblIndex = findLblFromCode(lbl, codeWithGoto)
        if lblIndex == None or i <= lblIndex:
            continue
        gotoLbl = getLabelFromGoto(codeWithGoto[lblIndex-1])
        gotoLblIndex = findLblFromCode(gotoLbl, codeWithGoto)
        if gotoLblIndex == None or lblIndex-1 >= gotoLblIndex:
            continue
        replaceList.append((lblIndex-1, i, lblIndex, gotoLblIndex))

    toFor = lambda x, y, z: "for(%s; %s; %s) {" % (x, y, z)
    for startIndex, endIndex, erase1, erase2 in replaceList:
        init = codeWithGoto[startIndex - 1].replace(';', '')
        end = codeWithGoto[endIndex - 2].replace(';', '')
        prop = re.sub(r'if\((.*)\) .*', r'\1', codeWithGoto[endIndex])

        codeWithGoto[startIndex - 1] = toFor(init, prop, end)
        codeWithGoto[startIndex] = ''
        codeWithGoto[endIndex - 2] = ''
        codeWithGoto[endIndex] = '}'

        codeWithGoto[erase1] = ''
        codeWithGoto[erase2] = ''

    return codeWithGoto

def main(argv):
    objdump = parser.inputCode(argv[1])
    funcs, addrs = parseObjdump.parseObjdumpOutput(objdump)
    ignores = [
            '_init',
            '.plt',
            '__libc_start_main@plt',
            '.plt.got',
            '_start',
            '__x86.get_pc_thunk.bx',
            'deregister_tm_clones',
            'register_tm_clones',
            '__do_global_dtors_aux',
            'frame_dummy',
            '__x86.get_pc_thunk.dx',
            '__x86.get_pc_thunk.ax',
            '__libc_csu_init',
            '__libc_csu_fini',
            '__x86.get_pc_thunk.si',
            '_fini',
            ]
    parser.initFuncLabel(addrs)
    for funcName in funcs:
        if funcName in ignores:
            continue
        insns = parser.parseObjdump(funcs[funcName])
        # print('\n'.join(list(map(str, insns))))

        parsedLines = parser.parse(insns)
        # print('\n'.join(list(map(str, parsedLines))))
        labeledAsm = insn.asm2LowIr(parsedLines)
        # print('\n'.join(list(map(str, labeledAsm))))
        # print("+++++++++++++++++++++")
        codeWithGoto = decLabeledAsm(funcName, labeledAsm)
        # print('\n'.join(codeWithGoto))
        code = ctlSt(codeWithGoto)
        code = list(filter(lambda x: x != '', code))
        print('\n'.join(code))
    
if __name__ == '__main__':
    import sys
    main(sys.argv)
