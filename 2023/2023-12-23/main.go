package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
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
	copy := State{st.pos, make(map[Position]bool, len(st.path)), st.dist}
	for p, b := range st.path {
		copy.path[p] = b
	}
	return &copy
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
	key := [2]int64{nodes[start].mask, visited}
	if result, ok := dfscache[key]; ok {
		return result
	}

	longest := -1 << 63 // the end point might not be the end node
	for nghbr, dist := range nodes[start].nghbrs {
		if nodes[nghbr].mask&visited != 0 {
			continue
		}
		longest = max(longest, dist+nodes.dfs(nghbr, end, visited|nodes[nghbr].mask))
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

		dirs := Cardinals
		if slides {
			switch route[state.pos.x][state.pos.y] {
			case '>':
				dirs = []Position{{0, 1}}
			case '<':
				dirs = []Position{{0, -1}}
			case 'v':
				dirs = []Position{{1, 0}}
			case '^':
				dirs = []Position{{-1, 0}}
			}
		}

		for _, dir := range dirs {
			nghbr := state.copy()
			nghbr.pos.x += dir.x
			nghbr.pos.y += dir.y

			if nghbr.pos.x < 0 {
				continue //only posible at start
			}

			r:= route[nghbr.pos.x][nghbr.pos.y]
			if slides {
				if (r == '>' && dir.y == -1) ||
					(r == '<' && dir.y == 1) ||
					(r == '^' && dir.x == 1) ||
					(r == 'v' && dir.x == -1) {
					continue
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
