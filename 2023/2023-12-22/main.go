package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"slices"
	"strconv"
	"strings"
)

type Brick struct {
	start [3]int
	end   [3]int
	name  rune
}
type BrickList []Brick
type BrickSet map[rune]bool

type RelationMap map[rune]BrickSet
type Wall map[[3]int]rune

type Support struct {
	brick  rune
	height int
}

func position(s string) [3]int {
	var r [3]int
	for i, num := range strings.Split(s, ",") {
		r[i], _ = strconv.Atoi(num)
	}
	return r
}

func parse(file string) (BrickList, error) {
	f, err := os.Open(file)
	if err != nil {
		return nil, err
	}
	defer f.Close()

	scanner := bufio.NewScanner(f)
	var bricks BrickList
	name := 'A'
	for scanner.Scan() {
		tokens := strings.Split(scanner.Text(), "~")
		brick := Brick{position(tokens[0]), position(tokens[1]), name}
		bricks = append(bricks, brick)
		name++
	}
	slices.SortFunc(bricks, func(a, b Brick) int {
		if min(a.start[2], a.end[2]) < min(b.start[2], b.end[2]) {
			return -1
		}
		if min(a.start[2], a.end[2]) == min(b.start[2], b.end[2]) {
			return 0
		}
		return 1
	})
	return bricks, nil
}

func drop(bricks BrickList) Wall {
	heights := make(map[[2]int]Support)
	wall := make(Wall, 0)
	for _, brick := range bricks {
		h := 0
		y := brick.start[1]
		for x := min(brick.start[0], brick.end[0]); x <= max(brick.start[0], brick.end[0]); x++ {
			support := heights[[2]int{x, y}]
			h = max(h, support.height)
		}
		x := brick.start[0]
		for y := min(brick.start[1], brick.end[1]); y <= max(brick.start[1], brick.end[1]); y++ {
			support := heights[[2]int{x, y}]
			h = max(h, support.height)
		}
		support := heights[[2]int{x, y}]
		support.height = h + 1 + max(brick.start[2], brick.end[2]) - min(brick.start[2], brick.end[2])
		support.brick = brick.name
		for x = min(brick.start[0], brick.end[0]); x <= max(brick.start[0], brick.end[0]); x++ {
			for y = min(brick.start[1], brick.end[1]); y <= max(brick.start[1], brick.end[1]); y++ {
				heights[[2]int{x, y}] = support
				for z := 0; z <= max(brick.start[2], brick.end[2])-min(brick.start[2], brick.end[2]); z++ {
					wall[[3]int{x, y, h + 1 + z}] = support.brick
				}
			}
		}
	}
	return wall
}

func relations(bricks []Brick, wall Wall) (RelationMap, RelationMap) {
	dependencies := make(RelationMap, len(bricks))
	dependents := make(RelationMap, len(bricks))
	for pos, supported := range wall {
		brick := wall[[3]int{pos[0], pos[1], pos[2] - 1}]
		if supported != brick && brick != 0 {
			if dependencies[supported] == nil {
				dependencies[supported] = make(BrickSet)
			}
			dependencies[supported][brick] = true
			if dependents[brick] == nil {
				dependents[brick] = make(BrickSet)
			}
			dependents[brick][supported] = true
		}
	}
	return dependencies, dependents
}

func disintegratable(bricks BrickList, dependencies RelationMap, dependents RelationMap) int {
	cnt := 0
	for _, b := range bricks {
		unsupported := true
		for k := range dependents[b.name] {
			if len(dependencies[k]) == 1 {
				unsupported = false
				break
			}
		}
		if unsupported {
			cnt++
		}
	}
	return cnt
}

func (start Brick) disintegrated(dependencies RelationMap, dependents RelationMap) int {
	disintegrated := BrickSet{start.name: true}
	unsupported := disintegrated
	for len(unsupported) != 0 {
		next := make(BrickSet)
		for brick := range unsupported {
			for supported := range dependents[brick] {
				fallen := true
				for supporter := range dependencies[supported] {
					if _, ok := disintegrated[supporter]; !ok {
						fallen = false
						break
					}
				}
				if fallen {
					next[supported] = true
					disintegrated[supported] = true
				}
			}
		}
		unsupported = next
	}
	return len(disintegrated) - 1
}

func (bricks BrickList) disintegrated(dependencies RelationMap, dependents RelationMap) int {
	cnt := 0
	for _, brick := range bricks {
		cnt += brick.disintegrated(dependencies, dependents)
	}
	return cnt
}

func main() {
	//file := "test.dat"
	file := "2023-12-22.dat"

	bricks, err := parse(file)
	if err != nil {
		log.Fatal(err)
	}
	wall := drop(bricks)
	dependencies, dependents := relations(bricks, wall)
	fmt.Println("part 1:", disintegratable(bricks, dependencies, dependents))
	fmt.Println("part 2:", bricks.disintegrated(dependencies, dependents))
}
