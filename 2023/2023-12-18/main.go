package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
)

const (
	Up = iota
	Right
	Down
	Left
)

const (
	Alternate = iota
	Unchanged
	Interior
)

type Tile struct {
	x int
	y int
}

func fill(lavas *map[Tile]int) int {
	minX := 0
	minY := 0
	maxX := 0
	maxY := 0
	for t := range *lavas {
		minX = min(minX, t.x)
		minY = min(minY, t.y)
		maxX = max(maxX, t.x)
		maxY = max(maxY, t.y)
	}

	for x := minX; x <= maxX; x++ {
		inside := false
		for y := minY; y <= maxY; y++ {
			tile := Tile{x, y}
			terrain, ok := (*lavas)[tile]
			if ok && terrain == Alternate {
				inside = !inside
			}
			if !ok && inside {
				(*lavas)[tile] = Interior
			}
		}
	}
	return len(*lavas)
}

func abs(x int) int {
	if x < 0 {
		return -x
	}
	return x
}

func area(corners []Tile) int {
	area := 0
	perimeter := 0
	for len(corners) > 1 {
		area += (corners[1].x - corners[0].x) * (corners[0].y + corners[1].y)
		perimeter += abs(corners[1].x-corners[0].x) + abs(corners[1].y-corners[0].y)
		
		corners = corners[1:]
	}
	area /= 2
	return abs(area) + perimeter/2 + 1
}

var lut = map[string]int{"0": Right, "1": Down, "2": Left, "3": Up}

func parseCorners(file string) ([]Tile, error) {
	f, err := os.Open(file)
	if err != nil {
		return nil, err
	}
	defer f.Close()

	scanner := bufio.NewScanner(f)
	corner := Tile{0, 0}
	corners := []Tile{corner}
	for scanner.Scan() {
		tokens := strings.Fields(scanner.Text())
		dir := lut[tokens[2][7:8]]
		dist64, err := strconv.ParseInt(tokens[2][2:7], 16, 64)
		if err != nil {
			return corners, err
		}
		dist := int(dist64)

		// update corners
		switch dir {
		case Up:
			corner.x -= dist
		case Down:
			corner.x += dist
		case Right:
			corner.y += dist
		case Left:
			corner.y -= dist
		}
		corners = append(corners, corner)
	}
	return corners, nil
}

func parseLavas(file string) (map[Tile]int, error) {
	f, err := os.Open(file)
	if err != nil {
		return nil, err
	}
	defer f.Close()

	scanner := bufio.NewScanner(f)
	lavas := make(map[Tile]int, 0)
	tile := Tile{0, 0}
	start := ""
	dir := ""
	for scanner.Scan() {
		tokens := strings.Fields(scanner.Text())
		dir = tokens[0]
		dist, err := strconv.Atoi(tokens[1])
		if err != nil {
			return lavas, err
		}
		if start == "" {
			start = tokens[0]
		}

		// update tiles
		for i := 0; i < dist; i++ {
			if i == 0 {
				if dir == "D" {
					lavas[tile] = Alternate
				} else if dir == "U" {
					lavas[tile] = Unchanged
				}
			}

			switch dir {
			case "U":
				tile.x -= 1
			case "D":
				tile.x += 1
			case "R":
				tile.y += 1
			case "L":
				tile.y -= 1
			}

			if dir == "U" || (dir == "D" && i != dist-1) {
				lavas[tile] = Alternate
			} else {
				lavas[tile] = Unchanged
			}
		}
	}
	if dir == "U" {
		lavas[Tile{0, 0}] = Alternate
	}
	return lavas, nil
}

func main() {
	// file := "test.dat"
	file := "2023-12-18.dat"

	lavas, err := parseLavas(file)
	if err != nil {
		log.Fatal(err)
	}
	corners, err := parseCorners(file)
	if err != nil {
		log.Fatal(err)
	}
	fmt.Println("part 1:", fill(&lavas))
	fmt.Println("part 2:", area(corners))
	// fmt.Println("part 2:", astar(lavas, starts, target, 4, 10))
}
