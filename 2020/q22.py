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
        p1, p2 = game(cards_a[1:cards_a[0] + 1], cards_b[1:cards_b[0] + 1], part_two_win)
        return len(p1) > 0

    result = cards_a[0] > cards_b[0]
    return result


def game(player1, player2, win):
    rnd = 0
    previous = set()
    while player1 and player2:
        key = tuple(player1 + [''] + player2)
        if key in previous:
            print('loop!')
            return player1, player2
        previous.add(key)

        rnd += 1
        print('round', rnd)

        if win(player1, player2):
            player1 += [player1.pop(0), player2.pop(0)]
        else:
            player2 += [player2.pop(0), player1.pop(0)]

    if player1:
        print('Player 1 wins game')
    else:
        print('Player 2 wins game')
    print()
    return player1, player2


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
    return deal['Player 1'], deal['Player 2']


player1, player2 = deal_cards(filename)
player1, player2 = game(player1, player2, part_two_win)

print()
print('end of game')
print('player1', sum([s*c for s, c in enumerate(reversed(player1), start=1)]), player1)
print('player2', sum([s*c for s, c in enumerate(reversed(player2), start=1)]), player2)
