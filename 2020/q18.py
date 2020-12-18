#!/usr/bin/python3
import re
BALANCED_RE = r'\(([^\(\)]+)\)'
filename = 'q18.dat'


def compute_one(eqn):
    tokens = eqn.split(' ')
    while len(tokens) > 1:
        # print('tkn', tokens)
        start = ' '.join(tokens[0:3])
        tokens = [str(eval(start))] + tokens[3:]
    # print('cpt', eqn, '=', tokens[0])
    return tokens[0]


def compute_two(eqn):
    sums = [str(eval(e)) for e in eqn.split('*')]
    return str(eval('*'.join(sums)))


def compute_full(eqn):
    m = re.search(BALANCED_RE, eqn)
    while m:
        # print('eqn', eqn)
        sub_ans = compute_full(m.group(1))
        # print('sub', m.group(1), '=', sub_ans)
        eqn = re.sub(BALANCED_RE, sub_ans, eqn, 1)
        # print('nxt', eqn)
        m = re.search(BALANCED_RE, eqn)
    return compute_two(eqn)


s = 0
with open(filename, 'r') as f:
    for equation in [l.rstrip() for l in f.readlines()]:
        ans = compute_full(equation)
        print('eval', equation, '=', ans)
        s += int(ans)
print('sum', s)
