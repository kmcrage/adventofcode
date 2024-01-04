package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
)

type Node struct {
	Spring    string
	NumGroups int // how far through the target []int we are
	HashCount int //how many #s in the current string
}

func parseLine(line string, repeats int) (string, []int) {
	fields := strings.Fields(line)

	springs := strings.Repeat("?"+fields[0], repeats)[1:]
	cnts := strings.Split(strings.Repeat(","+fields[1], repeats)[1:], ",")
	targets := make([]int, len(cnts))
	for i, sz := range cnts {
		targets[i], _ = strconv.Atoi(sz)
	}

	return springs, targets
}

var cache map[Node]int

func processLine(line string, repeats int) int {
	start, target := parseLine(line, repeats)
	cache = make(map[Node]int, 0)
	return solver(Node{start + ".", 0, 0}, target)
}

func solver(node Node, target []int) int {
	if s, ok := cache[node]; ok {
		return s
	}

	result := 0
	if node.Spring == "" {
		if len(target) == 0 && node.HashCount == 0 {
			result = 1
		}
		cache[node] = result
		return result
	}

	switch node.Spring[0] {
	case '.':
		if node.HashCount == 0 {
			result = solver(Node{node.Spring[1:], node.NumGroups, 0}, target)
		} else if node.HashCount == target[0] {
			result = solver(Node{node.Spring[1:], node.NumGroups + 1, 0}, target[1:])
		}
	case '#':
		if len(target) > 0 && node.HashCount < target[0] {
			result = solver(Node{node.Spring[1:], node.NumGroups, node.HashCount + 1}, target)
		}
	case '?':
		result = (solver(Node{"." + node.Spring[1:], node.NumGroups, node.HashCount}, target) +
			solver(Node{"#" + node.Spring[1:], node.NumGroups, node.HashCount}, target))

	}
	cache[node] = result
	return result
}

func process(file string, repeats []int) ([]int, error) {
	sums := make([]int, len(repeats))

	f, err := os.Open(file)
	if err != nil {
		return sums, err
	}
	defer f.Close()

	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		for i, repeat := range repeats {
			sums[i] += processLine(scanner.Text(), repeat)
		}
	}	
	return sums, nil
}

func main() {
	// file := "test.dat"
	file := "2023-12-12.dat"

	parts, err := process(file, []int{1, 5})
	if err != nil {
		log.Fatal(err)
	}
	fmt.Println("part 1:", parts[0])
	fmt.Println("part 2:", parts[1])
}
