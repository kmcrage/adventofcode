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
	mask   int64
}

type NodeMap map[Position]*Node

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

func (nodes NodeMap) longestpath() int {
	fmt.Println("dfs...")
	var start, end Position
	for p := range nodes {
		if p.x == 0 {
			start = p
		}
		if p.x > end.x {
			end = p
		}
	}
	dfscache = make(map[[2]int64]int)
	return nodes.dfs(start, end, nodes[start].mask)
}

var dfscache map[[2]int64]int

func (nodes NodeMap) dfs(start, end Position, visited int64) int {
	if start == end {
		return 0
	}
	if result, ok := dfscache[[2]int64{nodes[start].mask, visited}]; ok {
		return result
	}

	longest := -1 << 63 // the end point might not be the end node
	for nghbr, dist := range nodes[start].nghbrs {
		if nodes[nghbr].mask&visited != 0 {
			continue
		}
		longest = max(longest, dist+nodes.dfs(nghbr, end, visited|nodes[nghbr].mask))
	}

	dfscache[[2]int64{nodes[start].mask, visited}] = longest
	return longest
}

func nodemap(route [][]rune, slides bool) NodeMap {
	nodes := make(map[Position]*Node, 0)
	for i := range route {
		for j, r := range route[i] {
			jnctn := 0
			if r == '.' && (i == 0 || i == len(route)-1) { // arrows
				jnctn = 4
			} else if r == '.' {
				jnctn = 0
				for dir := 0; dir < 4; dir++ {
					x := i
					y := j
					if dir%2 == 0 {
						x += 1 - dir
					} else {
						y += 2 - dir
					}
					if route[x][y] != '#' {
						jnctn++
					}
				}
			}
			if jnctn > 2 {
				pos := Position{i, j}
				nodes[pos] = &Node{pos, make(map[Position]int), 1 << len(nodes)}
			}
		}
	}

	fmt.Println("found", len(nodes), "nodes...")
	fmt.Println("finding edge lengths...")
	analyse(route, nodes, slides)
	return nodes
}

func analyse(route [][]rune, nodes map[Position]*Node, slides bool) {
	for pos := range nodes {
		if pos.x == len(route)-1 {
			continue
		}
		routes(pos, route, nodes, slides)
	}
}

func routes(start Position, route [][]rune, nodes map[Position]*Node, slides bool) {
	queue := make([]*State, 1, len(route))
	queue[0] = &State{start, make(map[Position]bool), 0}
	queue[0].path[start] = true

	for len(queue) > 0 {
		state := *queue[0]
		queue = queue[1:]

		if _, ok := nodes[state.pos]; ok && start != state.pos {
			dist, ok := nodes[start].nghbrs[state.pos]
			if ok {
				nodes[start].nghbrs[state.pos] = max(dist, len(state.path)-1)
			} else {
				nodes[start].nghbrs[state.pos] = len(state.path) - 1
			}
			continue
		}

		for dir := 0; dir < 4; dir++ {
			if slides {
				if (route[state.pos.x][state.pos.y] == '>' && dir != 1) ||
					(route[state.pos.x][state.pos.y] == '<' && dir != 3) ||
					(route[state.pos.x][state.pos.y] == 'v' && dir != 0) ||
					(route[state.pos.x][state.pos.y] == '^' && dir != 2) {
					continue
				}
			}
			nghbr := state.copy()
			if dir%2 == 0 {
				nghbr.pos.x += 1 - dir
			} else {
				nghbr.pos.y += 2 - dir
			}
			if nghbr.pos.x < 0 {
				continue //only posible at start
			}
			if slides {
				if (route[nghbr.pos.x][nghbr.pos.y] == '#') ||
					(route[nghbr.pos.x][nghbr.pos.y] == '>' && dir == 3) ||
					(route[nghbr.pos.x][nghbr.pos.y] == '<' && dir == 1) ||
					(route[nghbr.pos.x][nghbr.pos.y] == '^' && dir == 0) ||
					(route[nghbr.pos.x][nghbr.pos.y] == 'v' && dir == 2) {
					continue
				}
			}else {
				if route[nghbr.pos.x][nghbr.pos.y] == '#' {
					continue
				}
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
	nodemap1 := nodemap(route, true)
	fmt.Println("part 1:", nodemap1.longestpath())
	nodemap2 := nodemap(route, false)
	fmt.Println("part 2:", nodemap2.longestpath())
}
