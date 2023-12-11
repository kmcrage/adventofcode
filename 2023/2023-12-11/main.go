package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
)

func parse(file string) ([][2]int, map[int]bool, map[int]bool, error) {
	galaxies := make([][2]int, 0)
	rows := make(map[int]bool, 0)
	cols := make(map[int]bool, 0)

	f, err := os.Open(file)
	if err != nil {
		return galaxies, rows, cols, err
	}
	defer f.Close()

	scanner := bufio.NewScanner(f)
	x := 0
	for scanner.Scan() {
		line := scanner.Text()
		for y, v := range line {
			if v == '#' {
				galaxies = append(galaxies, [2]int{x, y})
				rows[x] = true
				cols[y] = true
			}
		}
		x++
	}
	return galaxies, rows, cols, nil
}

func linearDist(start, end int, seen map[int]bool, expand int) int {
	dist := 0
	for x := start; x < end; x++ {
		if _, ok := seen[x]; ok {
			dist++
		} else {
			dist += expand
		}
	}
	return dist
}

func sumDist(galaxies [][2]int, rows map[int]bool, cols map[int]bool, expand int) int {
	sum := 0
	for i, galA := range galaxies {
		for _, galB := range galaxies[i+1:] {
			sum += linearDist(galA[0], galB[0], rows, expand)
			sum += linearDist(min(galA[1], galB[1]), max(galA[1], galB[1]), cols, expand)
		}
	}
	return sum
}

func main() {
	// file := "test.dat"
	file := "2023-12-11.dat"

	galaxies, rows, cols, err := parse(file)
	if err != nil {
		log.Fatal(err)
	}
	fmt.Println("part 1:", sumDist(galaxies, rows, cols, 2))
	fmt.Println("part 2:", sumDist(galaxies, rows, cols, 1000000))
}
