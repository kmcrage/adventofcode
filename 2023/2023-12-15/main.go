package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"regexp"
	"slices"
	"strconv"
	"strings"
)

type Lens struct {
	label string
	focal int
}

func hash(s string) int {
	hash := 0
	for _, r := range s {
		hash = (hash + int(r)) * 17 % 256
	}
	return hash
}

func parse(file string) ([]string, error) {
	f, err := os.Open(file)
	if err != nil {
		return nil, err
	}
	defer f.Close()

	scanner := bufio.NewScanner(f)
	var steps []string
	for scanner.Scan() {
		steps = append(steps, strings.Split(scanner.Text(), ",")...)
	}
	return steps, nil
}

func sumHash(steps []string) int {
	sum := 0
	for _, step := range steps {
		sum += hash(step)
	}
	return sum
}

func score(boxes [][]Lens) int {
	sum := 0
	for i, lenses := range boxes {
		for l, lens := range lenses {
			sum += (i + 1) * (l + 1) * lens.focal
		}
	}
	return sum
}

func sumHashmap(steps []string) int {
	boxes := make([][]Lens, 256)
	parseRe := regexp.MustCompile(`(\S+)([=-])(\d*)`)

	for _, step := range steps {
		match := parseRe.FindStringSubmatch(step)
		label := match[1]
		box := hash(label)
		lensEqual := func(l Lens) bool {
			return l.label == label
		}

		if match[2] == "=" {
			focal, _ := strconv.Atoi(match[3])
			idx := slices.IndexFunc(boxes[box], lensEqual)
			if idx == -1 {
				boxes[box] = append(boxes[box], Lens{label, focal})
			} else {
				boxes[box][idx].focal = focal
			}

		} else {
			boxes[box] = slices.DeleteFunc(boxes[box], lensEqual)
		}

	}
	return score(boxes)
}

func main() {
	// file := "test.dat"
	file := "2023-12-15.dat"

	steps, err := parse(file)
	if err != nil {
		log.Fatal(err)
	}

	fmt.Println("part 1:", sumHash(steps))
	fmt.Println("part 2:", sumHashmap(steps))

}
