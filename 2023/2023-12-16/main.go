package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
)

type State struct {
	X         int
	Y         int
	Direction string
}

func parse(file string) ([][]rune, error) {
	f, err := os.Open(file)
	if err != nil {
		return nil, err
	}
	defer f.Close()

	scanner := bufio.NewScanner(f)
	var mirrors [][]rune
	for scanner.Scan() {
		line := scanner.Text()
		row := make([]rune, len(line))
		for j, r := range line {
			row[j] = r
		}
		mirrors = append(mirrors, row)
	}
	return mirrors, nil
}

func count(visited map[State]bool) int {
	tiles := make(map[State]bool, len(visited))
	for s := range visited {
		s.Direction = "N"
		tiles[s] = true
	}
	return len(tiles) - 1 
}

func energised(mirrors [][]rune, start State) int {
	visited := make(map[State]bool, len(mirrors) * len(mirrors[0]))
	queue := make([]State, 0, 4*len(visited))
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
			state.X -= 1
		case "S":
			state.X += 1
		case "E":
			state.Y += 1
		case "W":
			state.Y -= 1
		}
	
		if  0 > state.X || state.X >= len(mirrors) || 0 > state.Y || state.Y >= len(mirrors[0]) {
			continue
		}

		switch mirrors[state.X][state.Y] {
		case '|' :
			switch state.Direction {
			case "E", "W":
				state.Direction = "N"
				queue = append(queue, State{state.X, state.Y, "S"})
			}
		case '-' :
			switch state.Direction {
			case "N", "S":
				state.Direction = "E"
				queue = append(queue, State{state.X, state.Y, "W"})
			}
		case '/' :
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
		case '\\' :
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

func maximise(mirrors [][]rune) int {
	max := 0
	for i := range(mirrors) {
		e := energised(mirrors, State{i, -1, "E"})
		if e > max {
			max = e
		}
		e = energised(mirrors, State{i, len(mirrors[0]), "W"})
		if e > max {
			max = e
		}
	}
	for j := range(mirrors[0]) {
		e := energised(mirrors, State{-1, j, "S"})
		if e > max {
			max = e
		}
		e = energised(mirrors, State{len(mirrors), j, "N"})
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

	fmt.Println("part 1:", energised(mirrors, State{0, -1, "E"}))
	fmt.Println("part 2:", maximise(mirrors))

}
