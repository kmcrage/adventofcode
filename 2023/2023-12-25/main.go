package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"slices"
	"strings"
)

type NodeMap map[string]*Node
type Wire struct {
	from, to string
	weight   int
}
type Node struct {
	nghbrs map[string]bool
	weight int
}

func parse(file string) ([]Wire, error) {
	f, err := os.Open(file)
	if err != nil {
		return nil, err
	}
	defer f.Close()

	scanner := bufio.NewScanner(f)
	wires := make([]Wire, 0)
	for scanner.Scan() {
		tokens := strings.Split(scanner.Text(), ": ")
		from := tokens[0]
		for _, to := range strings.Split(tokens[1], " ") {
			wires = append(wires, Wire{from: from, to: to})
		}

	}
	return wires, nil
}

func cut(wires []Wire, snips []Wire) []Wire {
	subwires := make([]Wire, 0, len(wires)-len(snips))
OUTER:
	for _, wire := range wires {
		for _, snip := range snips {
			if snip.from == wire.from && snip.to == wire.to {
				continue OUTER
			}
		}
		subwires = append(subwires, wire)
	}
	return subwires
}

func network(wires []Wire) NodeMap {
	nodemap := make(NodeMap)
	for _, wire := range wires {
		if _, ok := nodemap[wire.from]; !ok {
			nodemap[wire.from] = &Node{nghbrs: make(map[string]bool)}
		}
		if _, ok := nodemap[wire.to]; !ok {
			nodemap[wire.to] = &Node{nghbrs: make(map[string]bool)}
		}
		nodemap[wire.from].nghbrs[wire.to] = true
		nodemap[wire.to].nghbrs[wire.from] = true
	}
	return nodemap
}

func flood(nodemap NodeMap, seed string) (NodeMap, NodeMap) {
	fill := make(NodeMap)
	queue := []string{seed}
	for len(queue) > 0 {
		node := queue[0]
		queue = queue[1:]
		fill[node] = nodemap[node]
		for nghbr := range nodemap[node].nghbrs {
			if _, ok := fill[nghbr]; ok {
				continue
			}
			queue = append(queue, nghbr)
		}
	}

	remainder := make(NodeMap)
	for n, node := range nodemap {
		if _, ok := fill[n]; !ok {
			remainder[n] = node
		}
	}

	return fill, remainder
}

func divisions(nodemap NodeMap) int {
	var seeds [2]string
	remainder := nodemap
	prod := 1
	var fill NodeMap
	for s := 0; s < 2; s++ {
		for seeds[s] = range remainder {
			break
		}
		fill, remainder = flood(remainder, seeds[s])
		if s == 0 && (len(remainder) == 0 || len(remainder) == len(nodemap)) {
			return 0
		}
		prod *= len(fill)
	}
	if len(remainder) > 0 {
		return 0
	}
	return prod
}

func (nodemap NodeMap) weight(seed string,w int) {
	fill := make(NodeMap)
	queue := []string{seed}
	for len(queue) > 0 {
		node := queue[0]
		queue = queue[1:]
		fill[node] = nodemap[node]
		for nghbr := range nodemap[node].nghbrs {
			if _, ok := fill[nghbr]; ok {
				continue
			}
			// use a large step for the weight because we are
			// averaging with ints
			nodemap[nghbr].weight = nodemap[node].weight + w
			queue = append(queue, nghbr)
		}
	}
}

func (nodemap NodeMap) blur(num int) {
	for i := 0; i < num; i++ {
		for node := range nodemap {
			sum := 0
			for nghbr := range nodemap[node].nghbrs {
				sum += nodemap[nghbr].weight
			}
			nodemap[node].weight = sum / len(nodemap[node].nghbrs)
		}
	}
}

func (nodemap NodeMap) toWires() []Wire {
	snips := make([]Wire, 0)
	for node := range nodemap {
		for nghbr := range nodemap[node].nghbrs {
			weight := abs(nodemap[node].weight - nodemap[nghbr].weight)
			snips = append(snips, Wire{node, nghbr, weight})
		}
	}
	slices.SortFunc(snips, func(a Wire, b Wire) int {
		if a.weight < b.weight {
			return 1
		} else if a.weight > b.weight {
			return -1
		}
		return 0
	})
	return snips
}

func (nodemap NodeMap) bridges() []Wire {
	var seed string
	for seed = range nodemap {
		break
	}
	nodemap.weight(seed, 100)
	nodemap.blur(3)
	snips := nodemap.toWires()
	fmt.Println("snips:", snips[:10])
	return snips[:6]
}

func abs(n int) int {
	if n < 0 {
		return -n
	}
	return n
}

func part1(wires []Wire) int {
	nodemap := network(wires)
	snips := nodemap.bridges()
	return divisions(network(cut(wires, snips)))
}

/*
func writedot(file string, wires []Wire) {
	fo, err := os.Create(file)
	if err != nil {
		panic(err)
	}
	defer fo.Close()
	fo.WriteString("graph G {\n")
	for _, wire := range wires {
		fo.WriteString(wire.from + " -> " + wire.to + ";\n")
	}
	fo.WriteString("}\n")

}
*/

func main() {
	// file := "test.dat"
	file := "2023-12-25.dat"

	wires, err := parse(file)
	if err != nil {
		log.Fatal(err)
	}
	/*
	writedot("2023-12-25.dot", wires)
	// neato 2023-12-25.dot -O -Tjpg
	// these wires found by obesrvation
	/*
		snips := []Wire{
			{"xft", "pzv", 0},
			{"dqf", "cbx", 0},
			{"sds", "hbr", 0},
		}
	*/
	fmt.Println("Part1:", part1(wires))

}
