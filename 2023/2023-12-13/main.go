package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
)

func isHorizReflection(pattern []string, h int) int {
	cnt := 0
	for _, row := range pattern {
		for k := 0; k < min(h, len(pattern[0])-h); k++ {
			if row[h-k-1] != row[h+k] {
				cnt++
			}
		}
	}
	return cnt
}

func isVertReflection(pattern []string, v int) int {
	cnt := 0
	for k := 0; k < min(v, len(pattern)-v); k++ {
		for c := range pattern[0] {
			if pattern[v-k-1][c] != pattern[v+k][c] {
				cnt++
			}
		}
	}
	return cnt
}

func processPattern(pattern []string) (int, int) {
	match := 0
	smudge := 0
	for i := 1; i < len(pattern[0]); i++ {
		switch isHorizReflection(pattern, i) {
		case 0:
			match += i
		case 1:
			smudge += i
		}
	}
	for i := 1; i < len(pattern); i++ {
		switch isVertReflection(pattern, i) {
		case 0:
			match += i * 100
		case 1:
			smudge += i * 100
		}
	}
	// fmt.Println()
	return match, smudge
}

func process(file string) (int, int, error) {
	f, err := os.Open(file)
	if err != nil {
		return 0, 0, err
	}
	defer f.Close()

	scanner := bufio.NewScanner(f)
	pattern := make([]string, 0)
	sum := 0
	smudge := 0
	for scanner.Scan() {
		line := scanner.Text()
		if line != "" {
			pattern = append(pattern, line)
			continue
		}
		
		ma, sm := processPattern(pattern)
		sum += ma
		smudge += sm
		pattern = make([]string, 0)
	}
	if len(pattern) > 0 {
		ma, sm := processPattern(pattern)
		sum += ma
		smudge += sm
	}
	return sum, smudge, nil
}

func main() {
	// file := "test.dat"
	file := "2023-12-13.dat"

	sum, smudge, err := process(file)
	if err != nil {
		log.Fatal(err)
	}
	fmt.Println("part 1:", sum)
	fmt.Println("part 2:", smudge)
}
