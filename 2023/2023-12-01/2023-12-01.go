package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"regexp"
	"strconv"
)

var (
	digitsRe = regexp.MustCompile("[0-9]")
	numsRe   = regexp.MustCompile("[0-9]|one|two|three|four|five|six|seven|eight|nine")
)

func numtoi(s string) int {
	nums := map[string]int{
		"one":   1,
		"two":   2,
		"three": 3,
		"four":  4,
		"five":  5,
		"six":   6,
		"seven": 7,
		"eight": 8,
		"nine":  9,
	}

	ret := 0
	if value, ok := nums[s]; ok {
		ret = value
	} else {
		ret, _ = strconv.Atoi(s)
	}
	return ret
}

func part1(line string) int {
	digits := digitsRe.FindAllString(line, -1)

	a, _ := strconv.Atoi(digits[0])
	b, _ := strconv.Atoi(digits[len(digits)-1])

	return a*10 + b
}

func part2(line string) int {
	var nums []int
	for {
		idx := numsRe.FindStringIndex(line)
		if idx == nil {
			break
		}
		nums = append(nums, numtoi(line[idx[0]:idx[1]]))
		line = line[idx[0]+1:]
	}
	c := nums[0]
	d := nums[len(nums)-1]
	return c*10 + d
}

func main() {
	file := "2023-12-01.dat"
	f, err := os.Open(file)
	if err != nil {
		log.Fatal(err)
	}
	// remember to close the file at the end of the program
	defer f.Close()

	scanner := bufio.NewScanner(f)
	sum1 := 0
	sum2 := 0
	for scanner.Scan() {
		line := scanner.Text()
		sum1 += part1(line)
		sum2 += part2(line)
	}
	fmt.Println("Part one: ", sum1)
	fmt.Println("Part two: ", sum2)
}
