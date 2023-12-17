package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
)

type Tile struct {
	X int
	Y int
}
type State struct {
	Position  Tile
	Direction string
}

func parse(file string) (map[Tile]rune, error) {
	f, err := os.Open(file)
	if err != nil {
		return nil, err
	}
	defer f.Close()

	scanner := bufio.NewScanner(f)
	mirrors := make(map[Tile]rune, 0)
	i := 0
	for scanner.Scan() {
		line := scanner.Text()
		for j, r := range line {
			mirrors[Tile{i,j}] = r
		}
		i++
	}
	return mirrors, nil
}

func count(visited map[State]bool) int {
	tiles := make(map[Tile]bool, len(visited))
	for s := range visited {
		tiles[s.Position] = true
	}
	return len(tiles) - 1
}

func energised(mirrors map[Tile]rune, start State) int {
	visited := make(map[State]bool, len(mirrors))
	queue := make([]State, 0, 4*len(mirrors))
	queue = append(queue, start)
	for len(queue) > 0 {
		state := queue[0]
		queue = queue[1:]
		if visited[state] {
			continue
		}
		visited[state] = true

		switch state.Direction {
		case "N":
			state.Position.X -= 1
		case "S":
			state.Position.X += 1
		case "E":
			state.Position.Y += 1
		case "W":
			state.Position.Y -= 1
		}

		mirror := mirrors[state.Position]

		switch mirror {
		case 0:
			continue
		case '|':
			switch state.Direction {
			case "E", "W":
				state.Direction = "N"
				queue = append(queue, State{state.Position, "S"})
			}
		case '-':
			switch state.Direction {
			case "N", "S":
				state.Direction = "E"
				queue = append(queue, State{state.Position, "W"})
			}
		case '/':
			switch state.Direction {
			case "N":
				state.Direction = "E"
			case "S":
				state.Direction = "W"
			case "E":
				state.Direction = "N"
			case "W":
				state.Direction = "S"
			}
		case '\\':
			switch state.Direction {
			case "N":
				state.Direction = "W"
			case "S":
				state.Direction = "E"
			case "E":
				state.Direction = "S"
			case "W":
				state.Direction = "N"
			}
		}
		queue = append(queue, state)
	}
	return count(visited)
}

func limits(mirrors map[Tile]rune) (int,int) {
	maxX := 0
	maxY := 0
	for tile := range mirrors {
		maxX = max(maxX, tile.X)
		maxY = max(maxY, tile.Y)
	}
	return maxX, maxY
}

func maximise(mirrors map[Tile]rune) int {
	maxX, maxY := limits(mirrors)
	max := 0
	for i := 0; i < maxX; i++ {
		e := energised(mirrors, State{Tile{i, -1}, "E"})
		if e > max {
			max = e
		}
		e = energised(mirrors, State{Tile{i, maxY}, "W"})
		if e > max {
			max = e
		}
	}
	for j := 0; j < maxY; j++ {
		e := energised(mirrors, State{Tile{-1, j}, "S"})
		if e > max {
			max = e
		}
		e = energised(mirrors, State{Tile{maxX, j}, "N"})
		if e > max {
			max = e
		}
	}
	return max
}

func main() {
	// file := "test.dat"
	file := "2023-12-16.dat"

	mirrors, err := parse(file)
	if err != nil {
		log.Fatal(err)
	}

	fmt.Println("part 1:", energised(mirrors, State{Tile{0, -1}, "E"}))
	fmt.Println("part 2:", maximise(mirrors))

}
