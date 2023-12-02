package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
)

func power_bag(line string) int {
	bag := map[string]int{
		"red":   0,
		"green": 0,
		"blue":  0,
	}

	for _, round := range strings.Split(strings.Split(line, ":")[1], ";") {
		for _, balls := range strings.Split(round, ",") {
			tokens := strings.Fields(balls)
			num, _ := strconv.Atoi(tokens[0])
			if bag[tokens[1]] < num {
				bag[tokens[1]] = num
			}
		}
	}

	game := 1
	for _, num := range bag {
		game *= num
	}
	return game
}

func check_bag(line string) int {
	bag := map[string]int{
		"red":   12,
		"green": 13,
		"blue":  14,
	}

	for _, round := range strings.Split(strings.Split(line, ":")[1], ";") {
		for _, balls := range strings.Split(round, ",") {
			tokens := strings.Fields(balls)
			num, _ := strconv.Atoi(tokens[0])
			if bag[tokens[1]] < num {
				return 0
			}
		}
	}
	game, _ := strconv.Atoi(strings.Fields(strings.Split(line, ":")[0])[1])
	return game
}

func main() {
	file := "2023-12-02.dat"
	//file := "test.dat"
	f, err := os.Open(file)
	if err != nil {
		log.Fatal(err)
	}
	// remember to close the file at the end of the program
	defer f.Close()

	part1 := 0
	part2 := 0
	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		line := scanner.Text()
		part1 += check_bag(line)
		part2 += power_bag(line)
	}
	fmt.Println("part 1:", part1)
	fmt.Println("part 2:", part2)
}
