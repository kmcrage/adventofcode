#!/usr/bin/python3
import re
filename = 'q16.dat'

ticket = {}
fields = {}
errors = 0
my_ticket = None

parse_fields = True
with open(filename, 'r') as f:
    for ln in f.readlines():
        ln = ln.rstrip()
        if ln == '':
            parse_fields = False
            continue

        if parse_fields:
            label = ln.split(':')[0]
            valid = set()
            for r in re.findall(r'(\d+)-(\d+)', ln):
                valid |= set(range(int(r[0]), int(r[1]) + 1))
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

# resolve unique names: make sure this always halts
resolved = [i for i in ticket if len(ticket[i]) == 1]
queue = []
while resolved:
    for i in resolved:
        for t in [t for t, nm in ticket.items() if len(nm) > 1]:
            ticket[t] -= ticket[i]
            if len(ticket[t]) == 1:
                queue.append(t)
    resolved = queue
    queue = []

print('ticket:', ticket)
print('my_ticket:', my_ticket)

product = 1
for pos in [pos for pos, nm in ticket.items() if nm.pop().startswith('departure')]:
    product *= my_ticket[pos]
print('product:', product)
