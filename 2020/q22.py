#!/usr/bin/python3

filename = 'q22.dat'


def part_one_win(cards_a, cards_b, *_):
    print('player 1:', cards_a)
    print('player 2:', cards_b)
    return cards_a[0] > cards_b[0]


def part_two_win(cards_a, cards_b):
    print('player 1:', cards_a)
    print('player 2:', cards_b)

    if len(cards_a) > cards_a[0] and len(cards_b) > cards_b[0]:
        print()
        print('start subgame', cards_a[0], cards_b[0])
        deal = {'Player 1': cards_a[1:cards_a[0] + 1],
                'Player 2': cards_b[1:cards_b[0] + 1]}
        result = game(deal, part_two_win)
        return result

    result = cards_a[0] > cards_b[0]
    return result


def game(deal, win):
    rnd = 0
    previous = set()
    while not [p for p in deal.keys() if len(deal[p]) == 0]:
        key = tuple(deal['Player 1'] + [''] + deal['Player 2'])
        if key in previous:
            print('loop!')
            return True
        previous.add(key)

        rnd += 1
        print('round', rnd)

        if win(deal['Player 1'],  deal['Player 2']):
            deal['Player 1'] += [deal['Player 1'].pop(0), deal['Player 2'].pop(0)]
        else:
            deal['Player 2'] += [deal['Player 2'].pop(0), deal['Player 1'].pop(0)]

    if deal['Player 1']:
        print('Player 1 wins game')
    else:
        print('Player 2 wins game')
    print()
    return len(deal['Player 1']) > 0


def deal_cards(file):
    deal = {}
    with open(file, 'r') as f:
        player = None
        for line in f.readlines():
            line = line.strip()
            if line == '':
                continue

            if line.startswith('Player'):
                player = line[:-1]
                deal[player] = []
                continue

            deal[player].append(int(line))
    return deal


cards = deal_cards(filename)
game(cards, part_two_win)

print()
print('end of game')
score = 0
for p, c in cards.items():
    print(p, 'cards:', c)
    score += sum([s*c for s, c in enumerate(reversed(c), start=1)])
print('score:', score)
