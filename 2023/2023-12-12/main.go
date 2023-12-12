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
	Spring     string
	NumQueries int
}

func (node Node) counter() []int {
	cnt := 0
	cnts := make([]int, 0)
	for _, c := range node.Spring {
		switch c {
		case '.':
			if cnt > 0 {
				cnts = append(cnts, cnt)
			}
			cnt = 0
		case '#':
			cnt += 1
		case '?':
			if cnt > 0 {
				cnts = append(cnts, cnt)
			}
			return cnts
		}
	}
	if cnt > 0 {
		cnts = append(cnts, cnt)
	}
	return cnts
}

func IntArrayEquals(a []int, b []int) bool {
	if len(a) != len(b) {
		return false
	}
	for i, v := range a {
		if v != b[i] {
			return false
		}
	}
	return true
}

func parseLine(line string, repeats int) (Node, []int) {
	fields := strings.Fields(line)

	springs := strings.Repeat("?"+fields[0], repeats)
	springs = springs[1:]

	cntsStr := strings.Repeat(","+fields[1], repeats)
	cntsStr = cntsStr[1:]

	cnts := strings.Split(cntsStr, ",")
	counts := make([]int, len(cnts))
	for i, sz := range cnts {
		counts[i], _ = strconv.Atoi(sz)
	}

	numQueries := 0
	for _, s := range springs {
		if s == '?' {
			numQueries++
		}
	}

	return Node{Spring: springs, NumQueries: numQueries}, counts
}

func processLine(line string, repeats int) int {
	start, target := parseLine(line, repeats)

	q := []Node{start}
	matches := 0
	visited := map[Node]bool{}
	for len(q) > 0 {
		node := q[0]
		q = q[1:] //dequeue first node in queue(fifo)
		if _, ok := visited[node]; ok {
			continue
		}
		visited[node] = true
		// goal := node.counter()
		// fmt.Println(node, goal, target)

		for _, c := range ".#" {
			qnode := node
			qnode.Spring = strings.Replace(qnode.Spring, "?", string(c), 1)
			qnode.NumQueries--
			goal := qnode.counter()
			if IntArrayEquals(goal, target) {
				matches++
				continue
			}
			if _, ok := visited[qnode]; !ok &&
				qnode.NumQueries >= 0 &&
				len(goal) <= len(target) {
				appendFlag := true
				if len(goal) > 0 && goal[len(goal)-1] > target[len(goal)-1] {
					appendFlag = false
				}
				if len(goal) > 1 && goal[len(goal)-2] != target[len(goal)-2] {
					appendFlag = false
				}
				if appendFlag {
					q = append(q, qnode)
				}
			}
		}
	}
	return matches
}

func process(file string, repeats int) (int, error) {
	f, err := os.Open(file)
	if err != nil {
		return 0, err
	}
	defer f.Close()

	scanner := bufio.NewScanner(f)
	sum := 0
	for scanner.Scan() {
		//println(sum)
		sum += processLine(scanner.Text(), repeats)
		// fmt.Println(sum)
	}
	return sum, nil
}

func main() {
	// file := "test.dat"
	file := "2023-12-12.dat"

	part1, err := process(file, 1)
	if err != nil {
		log.Fatal(err)
	}
	fmt.Println("part 1:", part1)
	/*
		part2, err := process(file, 5)
		if err != nil {
			log.Fatal(err)
		}
		fmt.Println("part 2:", part2)
	*/
}
