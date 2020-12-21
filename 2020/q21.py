#!/usr/bin/python3
import re
import pprint

filename = 'q21.dat'
allergens = {}
ingredients = {}

with open(filename, 'r') as f:
    for line in f.readlines():
        m = re.match(r'(.*) \(contains (.*)\)', line)
        ingredient_list = set(m.group(1).split(' '))
        for i in ingredient_list:
            if i in ingredients:
                ingredients[i] += 1
            else:
                ingredients[i] = 1

        for allergen in [a.strip() for a in m.group(2).split(',')]:
            if allergen in allergens:
                allergens[allergen] &= ingredient_list
            else:
                allergens[allergen] = ingredient_list.copy()

pprint.pprint(allergens)

unsafe = set()
for us in allergens.values():
    unsafe |= us
print(unsafe)

print('safe total:', sum([v for i, v in ingredients.items() if i not in unsafe]))

unresolved = set(allergens.keys())
while unresolved:
    for a, i in [(a, i) for a, i in allergens.items() if len(i) == 1]:
        unresolved.discard(a)
        for b, j in allergens.items():
            if a != b:
                allergens[b] = allergens[b].difference(i)

pprint.pprint(allergens)
dangerous = [(a, i.pop()) for a, i in allergens.items()]
dangerous = sorted(dangerous, key=lambda x: x[0])
dangerous = [d[1] for d in dangerous]
print('danger:', ','.join(dangerous))
