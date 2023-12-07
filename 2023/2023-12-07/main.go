package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"sort"
	"strconv"
	"strings"
)

var order = map[byte]int{
	'A': 14,
	'K': 13,
	'Q': 12,
	'J': 11,
	'T': 10,
	'9': 9,
	'8': 8,
	'7': 7,
	'6': 6,
	'5': 5,
	'4': 4,
	'3': 3,
	'2': 2,
}

type Hand struct {
	Cards string
	Sets  [2]int
	Bet   int
}

func handeval(cards string, wild bool) [2]int {
	m := make(map[rune]int, len(cards))
	jokers := 0
	for _, char := range cards {
		m[char] += 1
	}
	if wild {
		if j, ok := m['J']; ok {
			jokers = j
			m['J'] = 0
		}
	}
	vals := make([]int, 0, len(m))
	for _, val := range m {
		vals = append(vals, val)
	}

	// always want at least two values
	vals = append(vals, 0)
	sort.Ints(vals)
	return [2]int{vals[len(vals)-1] + jokers, vals[len(vals)-2]}
}

func parse(file string) []Hand {
	f, err := os.Open(file)
	if err != nil {
		log.Fatal(err)
	}
	defer f.Close()

	scanner := bufio.NewScanner(f)
	hands := make([]Hand, 0)
	for scanner.Scan() {
		fields := strings.Fields(scanner.Text())
		cards := fields[0]
		bet, _ := strconv.Atoi(fields[1])

		hands = append(hands, Hand{Cards: cards, Bet: bet})
	}
	return hands
}

func evalHands(handsP *[]Hand, wild bool) {
	hands := *handsP
	for h := 0; h < len(hands); h++ {
		hands[h].Sets = handeval(hands[h].Cards, wild)
	}
}

func score(hands []Hand) int {
	sortHands(&hands)
	
	score := 0
	for i, hand := range hands {
		score += (i + 1) * hand.Bet
	}
	return score
}

func sortHands(handsP *[]Hand) {
	hands := *handsP
	sort.Slice(*handsP, func(i, j int) bool {
		for s := 0; s < 2; s++ {
			if hands[i].Sets[s] != hands[j].Sets[s] {
				return hands[i].Sets[s] < hands[j].Sets[s]
			}
		}
		for c := 0; c < len(hands[i].Cards); c++ {
			if hands[i].Cards[c] != hands[j].Cards[c] {
				return order[hands[i].Cards[c]] < order[hands[j].Cards[c]]
			}
		}
		return false // never reach here
	})
}

func main() {
	// file := "test.dat"
	file := "2023-12-07.dat"
	hands := parse(file)

	evalHands(&hands, false)
	fmt.Println("part 1:", score(hands))

	// jokers wild, but low value
	order['J'] = 1
	evalHands(&hands, true)
	fmt.Println("part 2:", score(hands))

}
