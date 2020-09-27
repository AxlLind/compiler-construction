import sys
import difflib
import re

def normalize(a):
    normalizedId = 0
    pattern = "Identifier\\(%s#%s\\)"
    replace = "Identifier(%s#%s)"
    while True:
        match = re.search(pattern % (r"(\w*)", r"([0-9]*)"), a)
        if match is None:
            break
        name = match[1]
        num  = match[2]
        a = re.sub(
            pattern = pattern % (name, num),
            repl = replace % (name, "_" + str(normalizedId)),
            string = a)
        normalizedId += 1
    return a


def stripAll(a):
    lines = a.splitlines()
    lines = [line.strip("\t ") for line in lines]
    return ''.join(lines)

"""
Takes two strings containing the result of the --ast+ option.
It compares them after normalizing the identifiers. The comparison
ignores new lines and indentation. A diff is printed if they are
not equal. The diff does NOT ignore new lines.

"""
def compare(a,b, printDiff = True):
    a = normalize(a)
    b = normalize(b)

    if stripAll(a) != stripAll(b):
        if printDiff:
            diff = difflib.ndiff(
                a.splitlines(keepends=True),
                b.splitlines(keepends=True))
            print(''.join(diff))
        return False
    return True


def main():
    with open(sys.argv[1],'r') as f:
        a = f.read()
    with open(sys.argv[2],'r') as f:
        b = f.read()

    compare(a,b)

if __name__ == '__main__':
    main()
