#!/usr/bin/python3
import re
filename = 'q16.dat'

ticket = {}
fields = {}
with open(filename, 'r') as f:
    errors = 0
    parse_fields = True
    my_ticket = None
    for ln in f.readlines():
        ln = ln.rstrip()
        if ln == '':
            parse_fields = False
            continue

        if parse_fields:
            label = ln.split(':')[0]
            valid = set()
            ranges = re.findall(r'(\d+)-(\d+)', ln)
            for r in ranges:
                valid = valid.union(set(list(range(int(r[0]), int(r[1]) + 1))))
            fields[label] = valid

        else:
            if my_ticket is None and ',' in ln:
                my_ticket = [int(n) for n in ln.split(',')]

            for i, n in enumerate([int(n) for n in re.findall(r'\d+', ln)]):
                valid_fields = [k for k, v in fields.items() if n in v]
                if valid_fields:
                    if i not in ticket:
                        ticket[i] = set(valid_fields)
                    else:
                        ticket[i] &= set(valid_fields)
                else:
                    errors += n

# unresolved errors and names
print('errors:', errors)
print('raw ticket:', ticket)

# resolve unique names
while True:
    resolved = [i for i in ticket if len(ticket[i]) == 1]
    if len(resolved) == len(ticket):
        break
    for i in resolved:
        for t in [t for t in ticket if t != i]:
            ticket[t] = ticket[t].difference(ticket[i])

print('ticket:', ticket)
print('my_ticket:', my_ticket)

product = 1
for pos in [pos for pos, nm in ticket.items() if nm.pop().startswith('departure')]:
    product *= my_ticket[pos]
print('product:', product)