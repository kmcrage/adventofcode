package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
)

type Position struct {
	x int
	y int
}

type State struct {
	pos   Position
	steps int
}

func count(seen map[Position]int, plots [][]rune, maxSteps int) int {
	c := 0
	for _, steps := range seen {
		if steps <= maxSteps && steps%2 == maxSteps%2 { //tiles alternate
			c++
		}
	}
	return c
}

func safemod(num int, sz int) int {
	n := num % sz
	n = (n + sz) % sz
	return n
}

func search(plots [][]rune, start Position, maxSteps []int) []int {
	initial := State{pos: start, steps: 0}
	queue := []State{initial}
	seen := make(map[Position]int, len(plots)*len(plots[0]))

	results := make([]int, 0)
	for len(queue) > 0 {
		state := queue[0]
		queue = queue[1:]
		if _, ok := seen[state.pos]; ok {
			continue
		}
		seen[state.pos] = state.steps
		if state.steps >= maxSteps[len(results)]+1 {
			results = append(results, count(seen, plots, maxSteps[len(results)]))
			if len(results) == len(maxSteps) {
				return results
			}
		}

		for i := 0; i < 4; i++ {
			npos := state.pos
			if i%2 == 0 {
				npos.x += i - 1
			} else {
				npos.y += i - 2
			}
			if plots[safemod(npos.x, len(plots))][safemod(npos.y, len(plots[0]))] != '.' {
				continue
			}
			nghbr := State{pos: npos, steps: state.steps + 1}
			if nghbr.steps > maxSteps[len(maxSteps)-1] {
				continue
			}
			queue = append(queue, nghbr)
		}
	}
	return results
}

func parse(file string) ([][]rune, Position, error) {
	var start Position
	f, err := os.Open(file)
	if err != nil {
		return nil, start, err
	}
	defer f.Close()

	scanner := bufio.NewScanner(f)
	var plots [][]rune
	for scanner.Scan() {
		line := scanner.Text()
		row := make([]rune, len(line))
		for j, r := range line {
			row[j] = r
			if r == 'S' {
				start = Position{x: len(plots), y: j}
				row[j] = '.'
			}
		}
		plots = append(plots, row)
	}
	return plots, start, nil
}

func quadratic(data []int, sz int, t int) int {
	a := (data[2] + data[0] - 2*data[1]) / 2
	c := data[0]
	b := data[1] - a - c
	x := (t - sz/2) / sz
	return a*x*x + b*x + c
}
func main() {
	//file := "test.dat"
	file := "2023-12-21.dat"

	plots, start, err := parse(file)
	if err != nil {
		log.Fatal(err)
	}
	sz := len(plots)
	counts := search(plots, start, []int{64, sz / 2, sz/2 + sz, sz/2 + 2*sz, sz/2 + 3*sz})
	fmt.Println("part 1:", counts[0])
	fmt.Println("part 2:", quadratic(counts[1:], sz, 26501365))
}
