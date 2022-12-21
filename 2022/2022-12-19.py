#!/usr/bin/python3
import collections
import re

data_filename = "2022-12-19.dat"
# data_filename = "test.dat"
RESOURCES = ("ore", "clay", "obsidian", "geode")


def get_blueprints(filename):
    blueprints = []
    with open(filename, "r") as f:
        blueprint = None
        for line in f:
            line = line.strip()
            m = re.search("Blueprint (\d+):", line)
            if m:
                if blueprint:
                    blueprints.append(blueprint)
                blueprint = {"number": int(m[1]), "costs": {}}
            for clause in line.split("."):
                robot_type = re.search("\s(\S+) robot", clause)
                if not robot_type:
                    continue
                blueprint["costs"][robot_type[1]] = {}
                for resource in RESOURCES:
                    res = re.search(f"(\d+) {resource}", clause)
                    if res:
                        blueprint["costs"][robot_type[1]][resource] = int(res[1])
        if blueprint:
            blueprints.append(blueprint)

    # convert costs into tuples
    for blueprint in blueprints:
        numcosts = []
        for resource in RESOURCES:
            costs = tuple(
                blueprint["costs"][resource].get(cost, 0) for cost in RESOURCES
            )
            numcosts.append(costs)
        blueprint["numcosts"] = tuple(numcosts)

    return blueprints


def estimax(nhbr):
    time, _, _, gas = nhbr
    return gas + time * (time - 1) // 2


def bfs(blueprint, resources, robots, maxtime):
    """
    breadth first search
    the 'nodes' here are the creation of a new robot, so we are taking multiple timesteps

    Args:
        blueprint (_type_): _description_
        resources (_type_): _description_
        robots (_type_): _description_
        maxtime (_type_): _description_

    Returns:
        _type_: _description_
    """
    visited = {resources, robots}
    queue = collections.deque([(maxtime, resources, robots, 0)])
    max_costs = [0, 0, 0, 0]
    for cost in blueprint:
        for r in range(3):
            if cost[r] > max_costs[r]:
                max_costs[r] = cost[r]

    best = 0
    while queue:  # Creating loop to visit each node
        time, resources, robots, gas = nhbr = queue.popleft()
        if time <= 0 or estimax(nhbr) <= best:
            continue

        for r, cost in enumerate(blueprint):
            if r < 3 and robots[r] >= max_costs[r]:
                # don't make more robots than we can use
                continue

            nhbr_time = time - 1
            nhbr_gas = gas
            nhbr_resources = [
                resources[0] - cost[0],
                resources[1] - cost[1],
                resources[2] - cost[2],
                resources[3] - cost[3],
            ]
            nhbr_robots = robots
            if (
                (nhbr_resources[0] < 0 and nhbr_robots[0] == 0)
                or (nhbr_resources[1] < 0 and nhbr_robots[1] == 0)
                or (nhbr_resources[2] < 0 and nhbr_robots[2] == 0)
            ):
                continue

            # take enough time steps to create the next robot
            steps = 0
            for res in (0, 1, 2, 3):
                if nhbr_resources[res] < 0:
                    steps_r = -(nhbr_resources[res] // nhbr_robots[res])
                    if steps_r > steps:
                        steps = steps_r
            nhbr_time -= steps
            nhbr_resources = (
                nhbr_resources[0] + steps * robots[0],
                nhbr_resources[1] + steps * robots[1],
                nhbr_resources[2] + steps * robots[2],
                nhbr_resources[3] + steps * robots[3],
            )

            if nhbr_time <= 0:
                continue

            nhbr_resources = (
                nhbr_resources[0] + robots[0],
                nhbr_resources[1] + robots[1],
                nhbr_resources[2] + robots[2],
                nhbr_resources[3] + robots[3],
            )
            nhbr_robots = robots[:r] + (robots[r] + 1,) + robots[r + 1 :]

            if r == 3:
                nhbr_gas += nhbr_time
            nhbr = (nhbr_time, nhbr_resources, nhbr_robots, nhbr_gas)
            if nhbr not in visited and estimax(nhbr) > best:
                if nhbr_gas > best:
                    best = nhbr_gas
                visited.add(nhbr)
                queue.append(nhbr)
    return best


def part_one(blueprints, time):
    result = 0
    for num, blueprint in enumerate(blueprints, 1):
        resources, robots = (0, 0, 0, 0), (1, 0, 0, 0)
        score = bfs(blueprint["numcosts"], resources, robots, time)
        result += score * num
    print(f"part one: {result}")


def part_two(blueprints, time):
    result = 1
    for blueprint in blueprints:
        resources, robots = (0, 0, 0, 0), (1, 0, 0, 0)
        score = bfs(blueprint["numcosts"], resources, robots, time)
        result *= score
    print(f"part two: {result}")


blueprints = get_blueprints(data_filename)
part_one(blueprints, 24)
part_two(blueprints[:3], 32)
