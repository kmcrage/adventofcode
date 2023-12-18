package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"sync"
)

const (
	Right = 1
	Left = 3
)

const (
	North = iota
	East
	South
	West
)
type State struct {
	X         int
	Y         int
	Direction int
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
		s.Direction = North
		tiles[s] = true
	}
	return len(tiles) - 1
}

func energised(mirrors [][]rune, start State) int {
	visited := make(map[State]bool, len(mirrors)*len(mirrors[0]))
	queue := make([]State, 0, 4*len(visited))
	queue = append(queue, start)
	for len(queue) > 0 {
		state := queue[0]
		queue = queue[1:]
		for {
			if visited[state] {
				break
			}
			visited[state] = true

			if state.Direction%2 == 0 {
				state.X += state.Direction - 1
			} else {
				state.Y += 2 - state.Direction
			}

			if 0 > state.X || state.X >= len(mirrors) || 0 > state.Y || state.Y >= len(mirrors[0]) {
				break
			}

			switch mirrors[state.X][state.Y] {
			case '|':
				if state.Direction%2 == 1{
					state.Direction = North
					queue = append(queue, State{state.X, state.Y, South})
				}
			case '-':
				if state.Direction%2 == 0{
					state.Direction = East
					queue = append(queue, State{state.X, state.Y, West})
				}
			case '/':
				if state.Direction%2 == 0{
					state.Direction = (state.Direction + Right) % 4
				}else{
					state.Direction = (state.Direction + Left) % 4
				}
			case '\\':
				if state.Direction%2 == 0{
					state.Direction = (state.Direction + Left) % 4
				}else{
					state.Direction = (state.Direction + Right) % 4
				}
			}
		}
	}
	return count(visited)
}

func worker(ch chan<- int, wg *sync.WaitGroup, mirrors [][]rune, state State) {
	defer wg.Done()
	ch <- energised(mirrors, state)
}

func maximise(mirrors [][]rune) int {
	var wg sync.WaitGroup
	ch := make(chan int, 2*len(mirrors)+2*len(mirrors[0])) // Creating an buffered channel
	for i := range mirrors {
		wg.Add(2)
		go worker(ch, &wg, mirrors, State{i, -1, East})
		go worker(ch, &wg, mirrors, State{i, len(mirrors[0]), West})
	}
	for j := range mirrors[0] {
		wg.Add(2)
		go worker(ch, &wg, mirrors, State{-1, j, South})
		go worker(ch, &wg, mirrors, State{len(mirrors), j, North})
	}

	go func() {
		wg.Wait()
		close(ch) // Close the channel when all workers have finished
	}()

	mx := 0
	for val := range ch {
		mx = max(mx, val)
	}
	return mx
}

func main() {
	// file := "test.dat"
	file := "2023-12-16.dat"

	mirrors, err := parse(file)
	if err != nil {
		log.Fatal(err)
	}

	fmt.Println("part 1:", energised(mirrors, State{0, -1, East}))
	fmt.Println("part 2:", maximise(mirrors))

}
