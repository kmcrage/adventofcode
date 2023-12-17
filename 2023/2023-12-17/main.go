package main

import (
	"bufio"
	"container/heap"
	"fmt"
	"log"
	"os"
	"strconv"
)

const (
	North = iota
	East
	South
	West
)

const (
	Right = 1
	Left  = 3
)

// An Item is something we manage in a priority queue.
type Item struct {
	x         int
	y         int
	direction int // NESW
	result    int // The target result
	priority  int // The priority of the item in the queue.
	index     int // The index of the item in the heap.
}

type State struct {
	x         int
	y         int
	direction int
}

// A PriorityQueue implements heap.Interface and holds Items.
type PriorityQueue []*Item

func (pq PriorityQueue) Len() int { return len(pq) }

func (pq PriorityQueue) Less(i, j int) bool {
	return pq[i].priority < pq[j].priority
}

func (pq PriorityQueue) Swap(i, j int) {
	pq[i], pq[j] = pq[j], pq[i]
	pq[i].index = i
	pq[j].index = j
}

func (pq *PriorityQueue) Push(x any) {
	n := len(*pq)
	item := x.(*Item)
	item.index = n
	*pq = append(*pq, item)
}

func (pq *PriorityQueue) Pop() any {
	old := *pq
	n := len(old)
	item := old[n-1]
	old[n-1] = nil  // avoid memory leak
	item.index = -1 // for safety
	*pq = old[0 : n-1]
	return item
}

func abs(x int) int {
	if x < 0 {
		return -x
	}
	return x
}

func astar(lavas [][]int, starts []Item, target [2]int, minSteps int, maxSteps int) int {
	pq := make(PriorityQueue, len(starts), 4*len(lavas)*len(lavas[0]))
	for i, start := range starts {
		pq[i] = &start
	}
	heap.Init(&pq)

	visited := make(map[State]int, 4*len(lavas)*len(lavas[0]))
	for pq.Len() > 0 {
		move := *heap.Pop(&pq).(*Item)
		//fmt.Println("<", move)
		state := State{move.x, move.y, move.direction}
		if value, ok := visited[state]; ok && move.result >= value {
			continue
		}
		if move.x == target[0] && move.y == target[1] {
			return move.result
		}
		visited[state] = move.result

		for step := 0; step < maxSteps; step++ {
			if move.direction%2 == 0 {
				move.x += move.direction - 1
			} else {
				move.y += 2 - move.direction
			}
			if 0 > move.x || move.x >= len(lavas) || 0 > move.y || move.y >= len(lavas[0]) {
				break
			}
			move.result += lavas[move.x][move.y]
			if step < minSteps-1 {
				continue
			}

			move.priority = move.result + abs(target[0]-move.x) + abs(target[1]-move.y)
			for _, turn := range [2]int{Right, Left} {
				next := move
				next.direction = (next.direction + turn) % 4
				state := State{next.x, next.y, next.direction}
				if value, ok := visited[state]; ok && next.result >= value {
					continue
				}
				//fmt.Println(">", next)
				heap.Push(&pq, &next)
			}
		}
	}
	panic("Panic!")
}

func parse(file string) ([][]int, error) {
	f, err := os.Open(file)
	if err != nil {
		return nil, err
	}
	defer f.Close()

	scanner := bufio.NewScanner(f)
	var lavas [][]int
	for scanner.Scan() {
		line := scanner.Text()
		row := make([]int, len(line))
		for j, r := range line {
			lava, _ := strconv.Atoi(string(r))
			row[j] = lava
		}
		lavas = append(lavas, row)
	}
	return lavas, nil
}

func main() {
	// file := "test.dat"
	file := "2023-12-17.dat"

	lavas, err := parse(file)
	if err != nil {
		log.Fatal(err)
	}
	target := [2]int{len(lavas) - 1, len(lavas[0]) - 1}
	starts := make([]Item, 2)
	starts[0] = Item{0, 0, East, 0, 0, 0}
	starts[1] = Item{0, 0, South, 0, 0, 0}
	fmt.Println("part 1:", astar(lavas, starts, target, 1, 3))
	fmt.Println("part 2:", astar(lavas, starts, target, 4, 10))
}
