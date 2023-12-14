package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strings"
)

func rotate(rocks [][]rune) [][]rune {
	rotated := make([][]rune, len(rocks[0]))
	for i := range rocks[0] {
		rotated[i] = make([]rune, len(rocks))
		for j := range rocks {
			rotated[i][j] = rocks[len(rocks[0])-j-1][i]
		}
	}
	return rotated
}

func copyRocks(rocks [][]rune) [][]rune {
	c := make([][]rune, len(rocks))
	for i, row := range rocks {
		c[i] = row[:]
	}
	return c
}

func toString(rocks [][]rune) string {
	var sb strings.Builder
	for _, row := range rocks {
		for _, r := range row {
			sb.WriteRune(r)
		}
	}
	return sb.String()
}

func offsetPeriod(rocks [][]rune) (int, int) {
	previous := make(map[string]int, 0)

	cycles := 0
	previous[toString(rocks)] = cycles
	for {
		cycles++
		rocks = cycle(rocks)
		if p, ok := previous[toString(rocks)]; ok {
			return p, cycles - p
		}
		previous[toString(rocks)] = cycles
	}
}

func cycle(rocks [][]rune) [][]rune {
	for i := 0; i < 4; i++ {
		rollNorth(rocks)
		rocks = rotate(rocks)
	}
	return rocks
}

func rollNorth(rocks [][]rune) {
	for j := range rocks[0] {
		cube := -1
		for i := range rocks {
			switch rocks[i][j] {
			case '#':
				cube = i
			case 'O':
				if cube != i-1 {
					rocks[i][j] = '.'
					rocks[cube+1][j] = 'O'
				}
				cube++
			}
		}
	}
}

/*
func display(rocks [][]rune) {
	for _, row := range rocks {
		for _, r := range row {
			fmt.Print(string(r))
		}
		fmt.Println()
	}
}
*/

func load(rocks [][]rune) int {
	sum := 0
	for i, row := range rocks {
		for _, r := range row {
			if r == 'O' {
				sum += len(rocks) - i
			}
		}
	}
	return sum
}

func parse(file string) ([][]rune, error) {
	f, err := os.Open(file)
	if err != nil {
		return nil, err
	}
	defer f.Close()

	rocks := make([][]rune, 0)
	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		line := scanner.Text()
		row := make([]rune, len(line))
		for j, rock := range line {
			row[j] = rock
		}
		rocks = append(rocks, row)
	}
	return rocks, nil
}

func part1(rocks [][]rune) int {
	rocks = copyRocks(rocks)
	rollNorth(rocks)
	return load(rocks)
}

func part2(rocks [][]rune, iterations int) int {
	rocks1 := copyRocks(rocks)
	offset, period := offsetPeriod(rocks1)

	rocks1 = copyRocks(rocks)
	cycles := offset + (iterations-offset) % period
	for i := 0; i < cycles; i++ {
		rocks1 = cycle(rocks1)
	}
	return load(rocks1)
}

func main() {
	//file := "test.dat"
	file := "2023-12-14.dat"

	rocks, err := parse(file)
	if err != nil {
		log.Fatal(err)
	}
	fmt.Println("part 1:", part1(rocks))
	fmt.Println("part 2:", part2(rocks, 1000000000))

}
