package main

import (
	"fmt"
	"log"
	"os"
	"strings"
)

type Coord struct {
	X int
	Y int
}

type BB struct {
	Start Coord
	End   Coord
}

type Number struct {
	Value int
	BB
}

type Symbol struct {
	Chr rune
	Coord
}

func (bb BB) touching(coord Coord) bool {
	return bb.Start.X-1 <= coord.X && coord.X <= bb.End.X+1 && bb.Start.Y-1 <= coord.Y && coord.Y <= bb.End.Y+1
}

func appendNumber(numbers *[]Number, num int, start Coord, end Coord) {
	if num > 0 {
		*numbers = append(*numbers, Number{num, BB{start, end}})
	}
}


func parse(rows []string) ([]Number, []Symbol) {
	// make an map of numbers, each with a start and end
	numbers := make([]Number, 0, len(rows))
	// make a map of symbols, each with a position
	symbols := make([]Symbol, 0, len(rows))

	start := Coord{0, 0}
	end := Coord{0, 0}
	num := 0
	for i, row := range rows {
		for j, chr := range row {
			coord := Coord{i, j}
			switch chr {
			case '1', '2', '3', '4', '5', '6', '7', '8', '9', '0':
				if num == 0 {
					start = coord
				}
				num = num*10 + int(chr)
				end = coord
				continue

			case '.':
				appendNumber(&numbers, num, start, end)
				num = 0
				continue

			default:
				appendNumber(&numbers, num, start, end)
				num = 0
				symbols = append(symbols, Symbol{chr, coord})
			}
		}
		appendNumber(&numbers, num, start, end)
		num = 0
	}
	return numbers, symbols
}

func part1(numbers []Number, symbols []Symbol) int {
	result := 0
	for _, number := range numbers {
		for _, symbol := range symbols {
			if number.BB.touching(symbol.Coord) {
				result += number.Value
				break
			}
		}
	}
	return result
}

func part2(numbers []Number, symbols []Symbol) int {
	result := 0
	for _, symbol := range symbols {
		parts := make([]int, 0)
		for _, number := range numbers {
			if number.BB.touching(symbol.Coord) {
				parts = append(parts, number.Value)
			}
		}
		if len(parts) == 2 {
			result += parts[0] * parts[1]
		}
	}
	return result
}

func main() {
	file := "2023-12-03.dat"
	//file := "test.dat"
	dat, err := os.ReadFile(file)
	if err != nil {
		log.Fatal(err)
	}
	rows := strings.Split(string(dat), "\n")

	numbers, symbols := parse(rows)
	fmt.Println("part 1: ", part1(numbers, symbols))
	fmt.Println("part 2: ", part2(numbers, symbols))
}
