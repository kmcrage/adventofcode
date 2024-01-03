package main

import (
	"bufio"
	"fmt"
	"log"
	"maps"
	"os"
	"runtime/pprof"

	"github.com/edwingeng/deque/v2"
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
	startNode := nodes[start]
	key := [2]int64{startNode.mask, visited}
	if result, ok := dfscache[key]; ok {
		return result
	}

	longest := -1 << 63 // the end point might not be the end node
	for nghbr, dist := range startNode.nghbrs {
		nghbrNode := nodes[nghbr]
		if nghbrNode.mask&visited != 0 {
			continue
		}
		longest = max(longest, dist+nodes.dfs(nghbr, end, visited|nghbrNode.mask))
	}

	dfscache[key] = longest
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
				for _, dir := range Cardinals {
					if route[i+dir.x][j+dir.y] != '#' {
						jnctn++
					}
				}
			}
			if jnctn > 2 {
				pos := Position{i, j}
				nodes[pos] = &Node{pos, make(map[Position]int, 4), 1 << len(nodes)}
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
	queue := deque.NewDeque[*State]()
	path := map[Position]bool{start: true}
	queue.PushBack(&State{start, path, 0})

	for queue.Len() > 0 {
		state := queue.PopFront()

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
			queue.PushBack(nghbr)
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
