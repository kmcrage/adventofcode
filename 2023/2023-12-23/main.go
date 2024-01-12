package main

import (
	"bufio"
	"fmt"
	"log"
	"maps"
	"os"
	"sync"
)

type Position struct {
	x int
	y int
}

type State struct {
	pos     Position
	path    map[Position]bool
	visited int64
	dist    int
	depth   int
	levels  []int
}

type Node struct {
	pos   Position
	to    map[*Node]int
	from  map[*Node]bool
	mask  int64
	level int
}

type NodeMap struct {
	nodes  map[Position]*Node
	start  Position
	end    Position
	levels []int
}

var Cardinals = []Position{{-1, 0}, {0, -1}, {1, 0}, {0, 1}}

func (st *State) copy() *State {
	levels := make([]int, len(st.levels))
	copy(levels, st.levels)
	return &State{st.pos, maps.Clone(st.path), st.visited, st.dist, st.depth, levels}
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

func worker(ch chan<- int, wg *sync.WaitGroup, dist int, nm *NodeMap, node, end *Node, visited int64, levels []int) {
	defer wg.Done()
	ch <- dist + nm.rundfs(node, end, visited, levels)
}

func (nm *NodeMap) longestpath() int {
	fmt.Println("bfs + dfs...")
	var wg sync.WaitGroup
	ch := make(chan int, 1024) // Creating an buffered channel

	bfsMaxDepth := 8

	queue := make([]*State, 1)
	levels := make([]int, len(nm.levels))
	copy(levels, nm.levels)
	queue[0] = &State{pos: nm.start, levels: levels}

	longest := 0
	for len(queue) > 0 {
		state := *queue[0]
		queue = queue[1:]
		node := nm.nodes[state.pos]
		state.visited |= node.mask

		if state.depth >= bfsMaxDepth {
			wg.Add(1)
			go worker(ch, &wg, state.dist, nm, node, nm.nodes[nm.end], state.visited, state.levels)
			continue
		}
		if state.pos == nm.end {
			longest = max(longest, state.dist)
			continue
		}

		state.levels[node.level] -= 1

		for nghbr, dist := range node.to {
			if state.visited&nghbr.mask != 0 {
				continue
			}
			if state.levels[node.level] == 0 && nghbr.level > node.level {
				continue
			}
			nstate := state.copy()
			nstate.depth += 1
			nstate.dist += dist
			nstate.pos = nghbr.pos
			queue = append(queue, nstate)
		}
	}

	go func() {
		wg.Wait()
		close(ch) // Close the channel when all workers have finished
	}()

	for val := range ch {
		longest = max(longest, val)
	}
	return longest
}

func (nm *NodeMap) rundfs(start, end *Node, visited int64, levels []int) int {
	dfscache := make(map[[2]int64]int)
	dfslevels := make([]int, len(levels))
	copy(dfslevels, levels)

	var dfs func(nodes *NodeMap, start, end *Node, visited int64, levels []int) int
	dfs = func(nodes *NodeMap, start, end *Node, visited int64, levels []int) int {
		if start == end {
			return 0
		}
		key := [2]int64{start.mask, visited}
		if result, ok := dfscache[key]; ok {
			return result
		}
		levels[start.level] -= 1

		longest := -1 << 63 // the end point might not be the end node
		for nghbr, dist := range start.to {
			if nghbr.mask&visited != 0 {
				continue
			}
			if levels[start.level] == 0 && nghbr.level > start.level {
				continue
			}
			nlevels := make([]int, len(levels))
			copy(nlevels, levels)
			longest = max(longest, dist+dfs(nodes, nghbr, end, visited|nghbr.mask, nlevels))
		}

		dfscache[key] = longest
		return longest
	}
	return dfs(nm, start, end, visited, dfslevels)
}

func (nm *NodeMap) junctions(route [][]rune, slides bool) {
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
				nm.nodes[pos] = &Node{pos: pos,
					to:   make(map[*Node]int, 4),
					from: make(map[*Node]bool, 4),
					mask: 1 << len(nm.nodes)}
			}
		}
	}
}

func nodemap(route [][]rune, slides bool) *NodeMap {
	nm := &NodeMap{nodes: make(map[Position]*Node, len(route))}
	nm.junctions(route, slides)
	fmt.Println("found", len(nm.nodes), "nodes...")
	fmt.Println("finding edge lengths...")
	nm.analyse(route, slides)
	fmt.Println("finding levels...")
	nm.setlevels()
	return nm
}

// the level sets are equal numbers of nodes from the end.
// If we've visited all the nodes in a level set, it forms
// a boundary that we can't cross again, so we need to be on the
// "end" side of that boundary
func (nm *NodeMap) setlevels() {
	endNode := nm.nodes[nm.end]
	queue := []*Node{endNode}
	visited := make(map[Position]bool)

	for len(queue) > 0 {
		node := queue[0]
		queue = queue[1:]
		if _, ok := visited[node.pos]; ok {
			continue
		}
		visited[node.pos] = true

		for nghbr := range node.from {
			if _, ok := visited[nghbr.pos]; ok {
				continue
			}
			nghbr.level = node.level + 1
			queue = append(queue, nghbr)
		}
	}

	nm.levels = make([]int, len(nm.nodes))
	for _, node := range nm.nodes {
		nm.levels[node.level] += 1
	}
}

func (nm *NodeMap) analyse(route [][]rune, slides bool) {
	for pos := range nm.nodes {
		if pos.x > nm.end.x {
			nm.end = pos
		}
		if pos.x == 0 {
			nm.start = pos
		}
		if pos.x == len(route)-1 {
			continue
		}
		nm.routes(pos, route, slides)
	}
}

func (nm *NodeMap) routes(start Position, route [][]rune, slides bool) {
	queue := make([]*State, 1, len(nm.nodes))
	path := map[Position]bool{start: true}
	queue[0] = &State{start, path, 0, 0, 0, nil}

	for len(queue) > 0 {
		state := queue[0]
		node := nm.nodes[state.pos]
		queue = queue[1:]

		if _, ok := nm.nodes[state.pos]; ok && start != state.pos {
			dist := max(nm.nodes[start].to[node], len(state.path)-1)
			nm.nodes[start].to[node] = dist
			nm.nodes[node.pos].from[nm.nodes[start]] = true
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
	// f, _ := os.Create("day23.prof")
	// pprof.StartCPUProfile(f)
	// defer pprof.StopCPUProfile()
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
