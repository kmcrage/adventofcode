package main

import (
	"bufio"
	"fmt"
	"log"
	"maps"
	"os"
	"runtime/pprof"
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
	level  int
}

type NodeMap map[Position]*Node

var Cardinals = []Position{{-1, 0}, {0, -1}, {1, 0}, {0, 1}}

func (st *State) copy() *State {
	return &State{st.pos, maps.Clone(st.path), st.dist}
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
		route = append(route, []rune(scanner.Text()))
	}
	return route, nil
}

func (nodes NodeMap) longestpath() int {
	var start, end Position
	levels := make([]int, len(nodes))
	for p := range nodes {
		if p.x == 0 {
			start = p
		}
		if p.x > end.x {
			end = p
		}
		levels[nodes[p].level] += 1
	}
	dfscache = make(map[[2]int64]int)
	fmt.Println("dfs...")
	return nodes.dfs(start, end, nodes[start].mask, levels)
}

var dfscache map[[2]int64]int

func (nodes NodeMap) dfs(start, end Position, visited int64, levels []int) int {
	if start == end {
		return 0
	}
	startNode := nodes[start]
	key := [2]int64{startNode.mask, visited}
	if result, ok := dfscache[key]; ok {
		return result
	}
	levels[startNode.level] -= 1

	longest := -1 << 63 // the end point might not be the end node
	for nghbr, dist := range startNode.nghbrs {
		nghbrNode := nodes[nghbr]
		if nghbrNode.mask&visited != 0 {
			continue
		}
		// this is de-facto assuming the end node is furthest from the start 
		if levels[startNode.level] == 0 && nghbrNode.level < startNode.level {
			continue
		}
		nlevels := make([]int, len(levels))
		copy(nlevels, levels)
		longest = max(longest, dist+nodes.dfs(nghbr, end, visited|nghbrNode.mask, nlevels))
	}

	dfscache[key] = longest
	return longest
}

func nodemap(route [][]rune, slides bool) NodeMap {
	nodes := make(map[Position]*Node, len(route))
	for i := range route {
		for j, r := range route[i] {
			jnctn := 0
			if r == '.' && (i == 0 || i == len(route)-1) { // arrows
				jnctn = 4
			} else if r == '.' {
				jnctn = 0
				for _, dir := range Cardinals {
					if route[i+dir.x][j+dir.y] != '#' {
						jnctn++
					}
				}
			}
			if jnctn > 2 {
				pos := Position{i, j}
				nodes[pos] = &Node{pos: pos,
					nghbrs: make(map[Position]int, 4),
					mask:   1 << len(nodes)}
			}
		}
	}

	fmt.Println("found", len(nodes), "nodes...")
	fmt.Println("finding edge lengths...")
	analyse(route, nodes, slides)
	return nodes
}

// the level sets are equal numbers of nodes from the start
// if we've visited all the nodes in a level set, it forms
// a boundary that we can't cross again, so we need to be on the  
// "end" side of that boundary
//
func (nodes NodeMap) levels(start Position) {
	fmt.Println("finding levels...")
	startNode := nodes[start]
	queue := []*Node{startNode}
	visited := make(map[Position]bool)

	for len(queue)>0 {
		node := queue[0]
		queue = queue[1:]
		if _, ok := visited[node.pos]; ok {
			continue
		}
		visited[node.pos] = true

		for nghbr := range node.nghbrs {
			nghbrNode := nodes[nghbr]
			if _, ok := visited[nghbrNode.pos]; ok {
				continue
			}
			nghbrNode.level = node.level + 1
			queue = append(queue, nghbrNode)
		}
	}

}

func analyse(route [][]rune, nodes NodeMap, slides bool) {
	var start Position
	for pos := range nodes {
		if pos.x == 0 {
			start = pos
		}
		if pos.x == len(route)-1 {
			continue
		}
		routes(pos, route, nodes, slides)
	}
	nodes.levels(start)
}

func routes(start Position, route [][]rune, nodes map[Position]*Node, slides bool) {
	queue := make([]*State, 1, len(nodes))
	path := map[Position]bool{start: true}
	queue[0] = &State{start, path, 0}

	for len(queue) > 0 {
		state := queue[0]
		queue = queue[1:]

		if _, ok := nodes[state.pos]; ok && start != state.pos {
			dist := max(nodes[start].nghbrs[state.pos], len(state.path)-1)
			nodes[start].nghbrs[state.pos] = dist
			continue
		}

		for _, dir := range Cardinals {
			nghbr := state.copy()
			nghbr.pos.x += dir.x
			nghbr.pos.y += dir.y

			if nghbr.pos.x < 0 {
				continue //only posible at start
			}

			r := route[nghbr.pos.x][nghbr.pos.y]
			if slides {
				switch r {
				case '>':
					nghbr.path[nghbr.pos] = true
					nghbr.pos.y += 1
					r = route[nghbr.pos.x][nghbr.pos.y]
				case '<':
					nghbr.path[nghbr.pos] = true
					nghbr.pos.y -= 1
					r = route[nghbr.pos.x][nghbr.pos.y]
				case '^':
					nghbr.path[nghbr.pos] = true
					nghbr.pos.x -= 1
					r = route[nghbr.pos.x][nghbr.pos.y]
				case 'v':
					nghbr.path[nghbr.pos] = true
					nghbr.pos.x += 1
					r = route[nghbr.pos.x][nghbr.pos.y]
				}
			}

			if r == '#' || nghbr.path[nghbr.pos] {
				continue
			}
			nghbr.path[nghbr.pos] = true
			queue = append(queue, nghbr)
		}
	}
}

func main() {
	f, _ := os.Create("day23.prof")
	pprof.StartCPUProfile(f)
	defer pprof.StopCPUProfile()
	// go tool pprof -http=:8080 day23.prof

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
