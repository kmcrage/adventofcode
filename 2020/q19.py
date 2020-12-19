#!/usr/bin/python3
filename = 'q19.dat'

rules = {}
end_rules = {}

corrections = {"8": "42 | 42 8",
               "11": "42 31 | 42 11 31"
               }


def eval_rule_list(message, rule_list):
    if not message or not rule_list:
        return not message and not rule_list

    if rule_list[0] in end_rules:
        return end_rules[rule_list[0]] == message[0] and eval_rule_list(message[1:], rule_list[1:])
    else:
        for test in rules[rule_list[0]]:
            if eval_rule_list(message, test + rule_list[1:]):
                return True
    return False


cnt = 0
with open(filename, 'r') as f:
    for line in [l.rstrip() for l in f.readlines()]:

        if ':' in line:
            num, rule = line.split(':')
            if '"' in rule:
                end_rules[num] = eval(rule)
            else:
                if num in corrections:
                    rule = corrections[num]

                rules[num] = [r.strip().split(' ') for r in rule.strip().split('|')]

        elif line:
            if eval_rule_list(line, ["0"]):
                print(line, 'True')
                cnt += 1
            else:
                print(line, 'False')
print('true messages', cnt)
