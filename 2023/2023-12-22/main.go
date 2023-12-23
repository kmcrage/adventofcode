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
	above map[*Brick]bool
	below map[*Brick]bool
}

type BrickMap map[rune]*Brick

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

func parse(file string) (BrickMap, error) {
	f, err := os.Open(file)
	if err != nil {
		return nil, err
	}
	defer f.Close()

	scanner := bufio.NewScanner(f)
	bricks := make(BrickMap, 0)
	name := 'A'
	for scanner.Scan() {
		tokens := strings.Split(scanner.Text(), "~")
		bricks[name] = &Brick{start: position(tokens[0]), end: position(tokens[1]), name: name}
		name++
	}
	return bricks, nil
}

func (brickmap BrickMap) sorted() []*Brick {
	bricks := make([]*Brick, len(brickmap))
	b := 0
	for _, brick := range brickmap {
		bricks[b] = brick
		b++
	}

	slices.SortFunc(bricks, func(a, b *Brick) int {
		if min(a.start[2], a.end[2]) < min(b.start[2], b.end[2]) {
			return -1
		}
		if min(a.start[2], a.end[2]) == min(b.start[2], b.end[2]) {
			return 0
		}
		return 1
	})
	return bricks
}

func drop(bricks BrickMap) Wall {
	heights := make(map[[2]int]Support)
	wall := make(Wall, 0)
	for _, brick := range bricks.sorted() {
		h := 0
		xMin := min(brick.start[0], brick.end[0])
		xMax := max(brick.start[0], brick.end[0])
		yMin := min(brick.start[1], brick.end[1])
		yMax := max(brick.start[1], brick.end[1])
		zMin := min(brick.start[2], brick.end[2])
		zMax := max(brick.start[2], brick.end[2])

		for x := xMin; x <= xMax; x++ {
			for y := yMin; y <= yMax; y++ {
				support := heights[[2]int{x, y}]
				h = max(h, support.height)
			}
		}

		support := heights[[2]int{xMin, yMin}]
		support.height = h + 1 + zMax - zMin
		support.brick = brick.name
		for x := xMin; x <= xMax; x++ {
			for y := yMin; y <= yMax; y++ {
				heights[[2]int{x, y}] = support
				for z := 0; z <= zMax -zMin; z++ {
					wall[[3]int{x, y, h + 1 + z}] = support.brick
				}
			}
		}
	}
	return wall
}

func relations(bricks BrickMap, wall Wall) {
	for pos, above := range wall {
		below := wall[[3]int{pos[0], pos[1], pos[2] - 1}]
		if below != 0 && above != below {
			if bricks[above].below == nil {
				bricks[above].below = make(map[*Brick]bool, 0)
			}
			bricks[above].below[bricks[below]] = true
			if bricks[below].above == nil {
				bricks[below].above = make(map[*Brick]bool, 0)
			}
			bricks[below].above[bricks[above]] = true
		}
	}
}

func (bricks BrickMap) disintegratable() int {
	cnt := 0
	for _, brick := range bricks {
		if brick.disintegratable() {
			cnt++
		}
	}
	return cnt
}

func (brick Brick) disintegratable() bool {
	for above := range brick.above {
		if len(above.below) == 1 {
			return false
		}
	}
	return true
}

func (start Brick) disintegrated() int {
	queue := []*Brick{&start}
	disintegrated := make(map[rune]bool, 0)
	for len(queue) > 0 {
		brick := queue[0]
		queue = queue[1:]
		disintegrated[brick.name] = true
		for above := range brick.above {
			if disintegrated[above.name] {
				continue
			}

			unsupported := true
			for supporter := range above.below {
				if !disintegrated[supporter.name] {
					unsupported = false
					break
				}
			}
			if unsupported {
				disintegrated[above.name] = true
				queue = append(queue, above)
			}
		}
	}
	return len(disintegrated) - 1
}

func (bricks BrickMap) disintegrated() int {
	cnt := 0
	for _, brick := range bricks {
		cnt += brick.disintegrated()
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
	relations(bricks, wall)
	fmt.Println("part 1:", bricks.disintegratable())
	fmt.Println("part 2:", bricks.disintegrated())
}
