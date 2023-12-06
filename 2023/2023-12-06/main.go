package main

import (
	"fmt"
	"log"
	"os"
	"regexp"
	"strconv"
	"strings"
)

var mergeRe = regexp.MustCompile(`(\d)\s+(\d)`) // note special quotes

func parseRow(row string, mode int) []int {
	items := make([]int, 0)
	if mode == 2 {
		row = mergeRe.ReplaceAllString(row, "$1$2")
	}
	for _, item := range strings.Fields(row)[1:] {
		i, _ := strconv.Atoi(item)
		items = append(items, i)
	}
	return items
}

func readRaces(file string, mode int) ([]int, []int) {
	dat, err := os.ReadFile(file)
	if err != nil {
		log.Fatal(err)
	}
	rows := strings.Split(string(dat), "\n")
	times := parseRow(rows[0], mode)
	distances := parseRow(rows[1], mode)
	return times, distances
}

func race(times []int, distances []int) int {
	result := 1
	for i, time := range times {
		bested := 0
		for t := 1; t < time; t++ {
			if t*(time-t) > distances[i] {
				bested++
			}
		}
		result *= bested
	}
	return result
}

func main() {
	//file := "test.dat"
	file := "2023-12-06.dat"

	times, distances := readRaces(file, 1)
	fmt.Println("part 1: ", race(times, distances))

	times, distances = readRaces(file, 2)
	fmt.Println("part 2: ", race(times, distances))
}
