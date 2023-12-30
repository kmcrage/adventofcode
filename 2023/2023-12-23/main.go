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
	pos  Position
	path map[Position]bool
	dist int
}

type Node struct {
	pos    Position
	nghbrs map[Position]int
}

func (st State) copy() State {
	copy := State{st.pos, make(map[Position]bool), st.dist}
	for p, b := range st.path {
		copy.path[p] = b
	}
	return copy
}

func parse(file string) ([][]rune, error) {
	f, err := os.Open(file)
	if err != nil {
		return nil, err
	}
	defer f.Close()

	scanner := bufio.NewScanner(f)
	route := make([][]rune, 0)
	for scanner.Scan() {
		line := scanner.Text()
		row := make([]rune, len(line))
		for j, r := range line {
			row[j] = r
		}
		route = append(route, row)
	}
	return route, nil
}

func dfs(route [][]rune, nodes map[Position]*Node) int {
	var start, end Position
	for p := range nodes {
		if p.x == 0 {
			start = p
		}
		if p.x > end.x {
			end = p
		}
	}

	queue := make([]State, 1)
	queue[0] = State{start, make(map[Position]bool), 0}
	longest := 0
	for len(queue) > 0 {
		state := queue[len(queue)-1]
		queue = queue[:len(queue)-1]
		if state.pos == end {
			longest = max(longest, state.dist)
			continue
		}

		for nghbr, dist := range nodes[state.pos].nghbrs {
			if state.path[nghbr] {
				continue
			}
			nstate := state.copy()
			nstate.pos = nghbr
			nstate.dist += dist
			nstate.path[nghbr] = true
			queue = append(queue, nstate)
		}
	}

	return longest
}

func nodemap(route [][]rune) map[Position]*Node {
	nodes := make(map[Position]*Node, 0)
	for i := range route {
		for j, r := range route[i] {
			jnctn := false
			if (r == '.' && (i == 0 || i == len(route)-1)) { // arrows
				jnctn = true
			}else if (i != 0 && i != len(route)-1 && r == '.'){
				jnctn = true
				for dir :=0;dir<4; dir++{
					x := i
					y := j
					if dir%2 == 0 {
						x += 1 - dir
					} else {
						y += 2 - dir
					}
					if route[x][y] == '.' {
						jnctn = false 
						break
					}
				}
			}
			if jnctn {
				pos := Position{i, j}
				nodes[pos] = &Node{pos, make(map[Position]int)}
			}
		}
	}
	return nodes
}

func analyse(route [][]rune, nodes map[Position]*Node) {
	for pos := range nodes {
		if pos.x == len(route)-1 {
			continue
		}
		routes(pos, route, nodes)
	}
}

func routes(start Position, route [][]rune, nodes map[Position]*Node) {
	queue := make([]*State, 1, len(route))
	queue[0] = &State{start, make(map[Position]bool), 0}
	queue[0].path[start] = true 

	for len(queue) > 0 {
		state := *queue[0]
		queue = queue[1:]

		_, ok := nodes[state.pos]
		if start != state.pos && ok {
			dist, ok := nodes[start].nghbrs[state.pos]
			if ok {
				nodes[start].nghbrs[state.pos] = max(dist, len(state.path)-1)
			} else {
				nodes[start].nghbrs[state.pos] = len(state.path)-1
			}
			continue
		}

		for dir := 0; dir < 4; dir++ {
			nghbr := state.copy()
			if dir%2 == 0 {
				nghbr.pos.x += 1 - dir
			} else {
				nghbr.pos.y += 2 - dir
			}
			if nghbr.pos.x < 0 {
				continue //only posible at start
			}
			if route[nghbr.pos.x][nghbr.pos.y] == '#'{
				continue
			}
			if nghbr.path[nghbr.pos] {
				continue
			}
			nghbr.path[nghbr.pos] = true
			queue = append(queue, &nghbr)
		}
	}
}

func main() {
	// file := "test.dat"
	file := "2023-12-23.dat"

	route, err := parse(file)
	if err != nil {
		log.Fatal(err)
	}
	fmt.Println("parsed...")
	nodemap := nodemap(route)
	fmt.Println("found nodes... ", len(nodemap))
	analyse(route, nodemap)
	for p,n := range nodemap {
		fmt.Println(p, *n)
	}
	fmt.Println("found edge lengths...")
	fmt.Println("part 2:", dfs(route,nodemap))
}