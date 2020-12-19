#!/usr/bin/python3
filename = 'q19.dat'

rules = {}


def eval_rule(message, rule_num):
    rule = rules[rule_num]
    # print('eval', rule, 'on', message)
    if '"' in rule:
        if len(message) and message[:1] == eval(rule):
            return message[1:]
        else:
            return False

    for test in rule.split('|'):
        msg = message
        for rule_num in test.strip().split(' '):
            msg = eval_rule(msg, rule_num)
            if msg is False:
                break
        else:
            return msg
    return False


cnt = 0
with open(filename, 'r') as f:
    for line in [l.rstrip() for l in f.readlines()]:

        if ':' in line:
            num, rule = line.split(':')
            if num not in rules:
                rules[num] = rule

        elif line:
            print(line, end='')
            if eval_rule(line, "0") == '':
                print('  true')
                cnt += 1
            else:
                print('  false')
print('true messages', cnt)
