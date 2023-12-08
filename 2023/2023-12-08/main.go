package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strings"
)

// greatest common divisor (GCD) via Euclidean algorithm
func GCD(a, b int) int {
	for b != 0 {
		a, b = b, a % b
	}
	return a
}

// find Least Common Multiple (LCM) via GCD
func LCM(a, b int, integers ...int) int {
	result := a * b / GCD(a, b)

	for _, i := range integers {
		result = LCM(result, i)
	}

	return result
}

func parse(file string) (string, map[string][2]string) {
	f, err := os.Open(file)
	if err != nil {
		log.Fatal(err)
	}
	defer f.Close()

	var path string
	nodes := make(map[string][2]string, 0)
	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		fields := strings.Fields(scanner.Text())
		switch len(fields) {
		case 1:
			path = fields[0]
			continue
		case 0:
			continue
		}

		node := fields[0]
		l := fields[2][1 : len(fields[2])-1]
		r := fields[3][:len(fields[3])-1]

		nodes[node] = [2]string{l, r}
	}
	return path, nodes
}

func matchNodes(nodes map[string][2]string, chr byte) []string {
	starts := make([]string, 0)
	for node := range nodes {
		if node[len(node)-1] == chr {
			starts = append(starts, node)
		}
	}
	return starts
}

func walkGhost(path string, nodes map[string][2]string) int {
	// currs = append(currs, matchNodes(nodes, 'Z')...)

	/*
		    Each --A node only reaches one --Z node in it's loop

		    they all reach their --Z at the same "step" in the directions every time,
			meaning the loops are all a consistent period instead of changing or branching

		    Conveniently the period it takes to reach the first --Z from the starting --A node
			is the same period as it takes to re-reach the --Z node when you're already there.
	*/

	currs := matchNodes(nodes, 'A')
	lcm := 1
	for _, node := range currs {
		dist := 0
		cycle := 0
		znode := ""
		for {
			for _, dir := range path {
				dist++
				switch dir {
				case 'L':
					node = nodes[node][0]
				case 'R':
					node = nodes[node][1]
				}
			}
			if node[len(node)-1] == 'Z' {
				if cycle == 0 {
					cycle = dist
					znode = node
					if cycle%len(path) != 0 {
						log.Fatal("cycle isn't multiple of path!")
					}
				} else {
					if znode != node {
						log.Fatal("not a simple loop, A->Z->Z!")
					}
					if 2*cycle != dist {
						log.Fatal("A->Z and Z->Z cycles are different!")
					}
					lcm = LCM(lcm, cycle)
					break
				}
			}
		}
	}
	return lcm
}

func walkMan(path string, nodes map[string][2]string) int {
	node := "AAA"
	dist := 0
	for {
		for _, dir := range path {
			dist++
			switch dir {
			case 'L':
				node = nodes[node][0]
			case 'R':
				node = nodes[node][1]
			}
			if node == "ZZZ" {
				return dist
			}
		}
	}
}
func main() {
	//file := "test.dat"
	file := "2023-12-08.dat"
	path, nodes := parse(file)

	fmt.Println("part 1:", walkMan(path, nodes))
	fmt.Println("part 2:", walkGhost(path, nodes))
}
