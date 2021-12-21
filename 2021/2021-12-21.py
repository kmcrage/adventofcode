#!/usr/bin/python3


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


def get_dice():
    dice = {}
    for i in [1, 2, 3]:
        for j in [1, 2, 3]:
            for k in [1, 2, 3]:
                s = i + j + k
                dice[s] = dice.get(s, 0) + 1
    return dice


def part_two(universe, win):
    dice = get_dice()
    player = 0
    while min(max(u[1], u[3]) for u in universe) < win:
        universe_prev = universe
        universe = {}
        for u, num_u in universe_prev.items():
            if u[1] >= win or u[3] >= win:
                universe[u] = universe.get(u, 0) + num_u
                continue

            for die, num_die in dice.items():
                v = list(u)
                v[2 * player] = (v[2 * player] + die - 1) % 10 + 1
                v[2 * player + 1] += v[2 * player]
                v = tuple(v)
                universe[v] = universe.get(v, 0) + num_die * num_u

        player = 1 - player
    return universe


part_one(1000)
print()

# [{pos_a, score_a , pos_b, score_b) : num of universes}, ....]
win = 21
universe = {(3, 0, 4, 0): 1}
universe = part_two(universe, win)
wins = [sum(n for u, n in universe.items() if u[p] >= win) for p in [1, 3]]
print("player one wins:", wins[0])
print("player two wins:", wins[1])
print("max wins:", max(wins))
