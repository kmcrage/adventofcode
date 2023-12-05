package main

import (
	"fmt"
	"log"
	"math"
	"os"
	"strconv"
	"strings"
)

type Map struct {
	Src   int
	Dst   int
	Range int
}

type Interval struct {
	Start int
	Range int
}

func readRows(file string) []string {
	dat, err := os.ReadFile(file)
	if err != nil {
		log.Fatal(err)
	}
	rows := strings.Split(string(dat), "\n")
	rows = append(rows, "")
	return rows
}

func parseRows(rows []string) ([]int, [][]Map) {
	seeds := make([]int, 0)
	maps := make([][]Map, 0, 7)
	var submap []Map
	for _, row := range rows {
		tokens := strings.Fields(row)
		switch l := len(tokens); l {
		case 3:
			dst, _ := strconv.Atoi(tokens[0])
			src, _ := strconv.Atoi(tokens[1])
			rng, _ := strconv.Atoi(tokens[2])
			submap = append(submap, Map{Src: src, Dst: dst, Range: rng})
			continue

		case 2:
			continue

		case 0:
			if submap != nil {
				maps = append(maps, submap)
			}
			submap = nil
			continue

		default:
			for _, seedStr := range tokens[1:] {
				seed, _ := strconv.Atoi(seedStr)
				seeds = append(seeds, seed)
			}
		}
	}

	return seeds, maps
}

func processSeed(seed int, maps []Map) int {
	for _, m := range maps {
		if m.Src <= seed && seed <= m.Src+m.Range {
			return m.Dst + seed - m.Src
		}
	}
	return seed
}

func part1(seeds []int, xforms [][]Map) int {
	minLocation := math.MaxInt
	for _, seed := range seeds {
		// fmt.Println(seed)
		for _, maps := range xforms {
			seed = processSeed(seed, maps)
		}
		minLocation = min(minLocation, seed)
	}
	return minLocation
}

func processIntervals(intervals []Interval, maps []Map) []Interval {
	processed := make([]Interval, 0, len(intervals))
	for len(intervals) > 0 {
		//fmt.Println("to process: ", intervals)
		//fmt.Println("to next phase: ", processed)
		interval := intervals[0]
		intervals = intervals[1:]

		for _, m := range maps {
			if interval.Start+interval.Range <= m.Src || m.Src+m.Range <= interval.Start {
				// missing completely: no change
				continue

			} else if m.Src <= interval.Start && interval.Start+interval.Range <= m.Src+m.Range {
				// completely inside the map: one interval to the next stage
				processed = append(processed,
					Interval{Start: m.Dst + interval.Start - m.Src, Range: interval.Range},
				)
				interval.Range = 0
				break

			} else if interval.Start < m.Src && m.Src+m.Range < interval.Start+interval.Range {
				// completely covers the map: three intervals, one interval to the next stage
				processed = append(processed,
					Interval{Start: m.Dst, Range: m.Range},
				)
				intervals = append(intervals,
					Interval{
						Start: m.Src + m.Range,
						Range: interval.Start + interval.Range - m.Src - m.Range,
					},
				)
				interval = Interval{Start: interval.Start, Range: m.Src - interval.Start}

			} else if interval.Start < m.Src {
				// partial coverage, interval lower: two intervals, one interval to the next stage
				processed = append(processed,
					Interval{Start: m.Dst, Range: interval.Range + interval.Start - m.Src},
				)
				interval = Interval{Start: interval.Start, Range: m.Src - interval.Start}

			} else {
				// partial coverage, interval higher: two intervals, one interval to the next stage
				processed = append(processed,
					Interval{
						Start: m.Dst + interval.Start - m.Src,
						Range: m.Src + m.Range - interval.Start,
					},
				)
				interval = Interval{
					Start: m.Src + m.Range,
					Range: interval.Start + interval.Range - m.Src - m.Range,
				}
			}
		}
		if interval.Range != 0 {
			processed = append(processed, interval)
		}
	}
	return processed
	//fmt.Println("stage end:", intervals)
}

func part2(seeds []int, xforms [][]Map) int {
	intervals := make([]Interval, len(seeds)/2)
	for i := 0; i < len(seeds); i += 2 {
		intervals[i/2] = Interval{Start: seeds[i], Range: seeds[i+1]}
	}

	for _, maps := range xforms {
		intervals = processIntervals(intervals, maps)
	}

	return minStart(intervals)
}

func minStart(intervals []Interval) int {
	minStart := math.MaxInt
	for _, interval := range intervals {
		minStart = min(minStart, interval.Start)
	}
	return minStart
}

func main() {
	//file := "test.dat"
	file := "2023-12-05.dat"

	rows := readRows(file)
	seeds, maps := parseRows(rows)
	fmt.Println("part 1: ", part1(seeds, maps))
	fmt.Println("part 2: ", part2(seeds, maps))
}
