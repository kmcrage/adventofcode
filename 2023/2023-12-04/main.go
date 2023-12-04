package main

import (
	"fmt"
	"log"
	"os"
	"strings"
)

func scoring(cards []string) []int {
	scores := make([]int, len(cards))
	for i, card := range cards {
		cardParts := strings.SplitN(card, "|", 2)
		winList := strings.Fields(strings.SplitN(cardParts[0], ":", 2)[1])
		wins := make(map[string]bool, len(winList))
		for _, win := range winList {
			wins[win] = true
		}

		for _, num := range strings.Fields(cardParts[1]) {
			if wins[num] {
				scores[i]++
			}
		}
	}
	return scores
}

func part1(scores []int) int {
	result := 0
	for _, score := range scores {
		if score > 0 {
			result += 1 << (score - 1)
		}
	}
	return result
}

func part2(scores []int) int {
	cards := make([]int, len(scores))
	for i := range cards {
		cards[i] = 1
	}

	result := 0
	for i, score := range scores {
		result += cards[i]

		for j := i + 1; j < i+1+score; j++ {
			cards[j] += cards[i]
		}
	}
	return result
}

func main() {
	file := "2023-12-04.dat"
	//file := "test.dat"
	dat, err := os.ReadFile(file)
	if err != nil {
		log.Fatal(err)
	}
	rows := strings.Split(string(dat), "\n")

	scores := scoring(rows)
	fmt.Println("part 1: ", part1(scores))
	fmt.Println("part 2: ", part2(scores))
}
