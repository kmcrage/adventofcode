package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
)

func allZeros(nums []int) bool {
	for _, num := range nums {
		if num != 0 {
			return false
		}
	}
	return true
}

func extra(nums []int) (int, int) {
	// simplify until we have all zeros....
	working := make([][]int, 1)
	working[0] = nums
	row := working[0]
	for !allZeros(row) {
		prev := row
		row = make([]int, len(prev)-1)
		for i := 0; i < len(prev)-1; i++ {
			row[i] = prev[i+1] - prev[i]
		}
		working = append(working, row)
	}

	// extend rows
	for i := len(working) - 2; i >= 0; i-- {
		end := working[i][len(working[i])-1] + working[i+1][len(working[i+1])-1]
		start := working[i][0] - working[i+1][0]
		working[i] = append([]int{start}, working[i]...)
		working[i] = append(working[i], end)
	}
	// return the start and end of the first row
	return working[0][0], working[0][len(working[0])-1]
}

func processSensor(line string) (int, int, error) {
	fields := strings.Fields(line)
	nums := make([]int, len(fields))
	for n, num := range fields {
		var err error
		nums[n], err = strconv.Atoi(num)
		if err != nil {
			return 0, 0, err
		}
	}
	start, end := extra(nums)
	return start, end, nil
}

func process(file string) (int, int, error) {
	f, err := os.Open(file)
	if err != nil {
		return 0, 0, err
	}
	defer f.Close()

	sumStart := 0
	sumEnd := 0
	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		start, end, err := processSensor(scanner.Text())
		if err != nil {
			return 0, 0, err
		}
		sumStart += start
		sumEnd += end
	}
	return sumStart, sumEnd, nil
}

func main() {
	// file := "test.dat"
	file := "2023-12-09.dat"

	sumStart, sumEnd, err := process(file)
	if err != nil {
		log.Fatal(err)
	}

	fmt.Println("part 1:", sumEnd)
	fmt.Println("part 2:", sumStart)
}
