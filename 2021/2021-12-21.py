#!/usr/bin/python3

from collections import Counter
import itertools


def part_one(win):
    pos = [3, 4]
    score = [0, 0]
    round = 0
    while max(score) < win:
        dice = 9 * round + 6
        player = round % 2
        pos[player] = (pos[player] + dice - 1) % 10 + 1
        score[player] += pos[player]

        round += 1

    print("score:", score)
    print("part one:", min(score) * 3 * round)


def get_dice(rolls):
    return Counter(sum(p) for p in itertools.product([1, 2, 3], repeat=rolls))


def part_two(multiverse, win):
    dice = get_dice(3)
    player = 0
    while min(max(u[p][1] for p in [0, 1]) for u in multiverse) < win:
        multiverse_prev = multiverse
        multiverse = {}
        for u, num_u in multiverse_prev.items():
            if any(u[p][1] >= win for p in [0, 1]):
                multiverse[u] = multiverse.get(u, 0) + num_u
                continue

            for die, num_die in dice.items():
                v = [None, None]
                v[1 - player] = u[1 - player]

                position = (u[player][0] + die - 1) % 10 + 1
                score = u[player][1] + position
                v[player] = (position, score)
                v = tuple(v)

                multiverse[v] = multiverse.get(v, 0) + num_die * num_u

        player = 1 - player
    return multiverse


part_one(1000)
print()

# [((pos_a, score_a) , (pos_b, score_b)) : num of universes, ....]
win = 21
multiverse = {((3, 0), (4, 0)): 1}
multiverse = part_two(multiverse, win)
wins = [sum(n for u, n in multiverse.items() if u[p][1] >= win) for p in [0, 1]]
print("player one wins:", wins[0])
print("player two wins:", wins[1])
print("max wins:", max(wins))
