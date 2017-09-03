import re

def isFuncHead(line):
    return re.match(r'[0-9a-fA-F]+ <.+>:', line) != None, re.sub(r'([0-9a-f]*) \<(.+)\>:', r'\2,\1', line)

def parseObjdumpOutput(lines):
    lines = list(map(lambda x: x.strip(), lines))
    # print('\n'.join(map(lambda x: "A" + x + "A", lines)))
    codes = {}
    addrs = {}
    index = 0
    while index < len(lines):
        isHead, funcNameAddr = isFuncHead(lines[index])
        index += 1
        if isHead:
            funcName, funcAddr = funcNameAddr.split(',')
            codes[funcName] = []
            addrs[funcName] = funcAddr
            line = lines[index]
            _index = index
            for line in lines[_index:]:
                index += 1
                if line == '':
                    break
                codes[funcName].append(line)

    return codes, addrs

# import sys
# res = parse(open(sys.argv[1]).readlines())
# # print('\n'.join(res))
# # print(res)
# for funcName in res:
#     print("func: %s" % funcName)
#     print('\n'.join(res[funcName]))
