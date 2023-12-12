package main

import (
	"bufio"
	"gonum.org/v1/gonum/stat/combin"
	"fmt"
	"log"
	"os"
	"regexp"
	"strconv"
	"strings"
)

var hashesRe = regexp.MustCompile(`#+`)
func counter(springs string) []int {
	hashIdxs := hashesRe.FindAllStringIndex(springs, -1)
	cnts := make([]int, len(hashIdxs))
	for i, hIdx := range hashIdxs {
		cnts[i] = hIdx[1] - hIdx[0]
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

func parseLine(line string) (string, []int, int){
	fields := strings.Fields(line)
	springs := fields[0]

	cnts := strings.Split(fields[1], ",")
	counts := make([]int,len(cnts))
	total := 0
	for i,sz := range cnts {
		counts[i], _ = strconv.Atoi(sz)
		total += counts[i]
	}

	numHash := 0
	for _,s := range springs {
		if s == '#' {
			numHash++
		}
	}

	return springs, counts, total - numHash
}

var qRe = regexp.MustCompile(`\?`)
func processLine(line string) int {
	springs, counts, choose := parseLine(line)

	qIndices := qRe.FindAllStringIndex(springs, -1)
	matches := 0
	for _,combi := range combin.Combinations(len(qIndices), choose) {
		spr := springs
		for _,idx := range combi {
			spr = spr[:qIndices[idx][0]] + "#" + spr[qIndices[idx][0]+1:]
		}
		if IntArrayEquals(counter(spr), counts) {
			matches++
		}
	}
	return matches
}

func process(file string) (int, error) {
	f, err := os.Open(file)
	if err != nil {
		return 0, err
	}
	defer f.Close()

	scanner := bufio.NewScanner(f)
	sum := 0
	for scanner.Scan() {
		sum += processLine(scanner.Text())
	}
	return sum, nil
}

func main() {
	// file := "test.dat"
	file := "2023-12-12.dat"

	part1, err := process(file)
	if err != nil {
		log.Fatal(err)
	}
	fmt.Println("part 1:", part1)
	// fmt.Println("part 2:", sumDist(galaxies, rows, cols, 1000000))
}
