package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
)

func runeInSet(r rune, set string) bool {
	for _, s := range set {
		if r == s {
			return true
		}
	}
	return false
}

func parse(file string) ([][]rune, [2]int, error) {
	maze := make([][]rune, 0)
	var start [2]int

	f, err := os.Open(file)
	if err != nil {
		return maze, start, err
	}
	defer f.Close()

	scanner := bufio.NewScanner(f)
	x := 0
	for scanner.Scan() {
		line := scanner.Text()
		maze = append(maze, make([]rune, len(line)))
		for y, m := range line {
			maze[x][y] = m

			if m == 'S' {
				start = [2]int{x, y}
			}
		}
		x += 1
	}
	return maze, start, nil
}

func initDir(maze [][]rune, x int, y int, prev string) string {
	if y > 0 && runeInSet(maze[x][y-1], "-LFS") && prev != "E" {
		return "W"
	}
	if x > 0 && runeInSet(maze[x-1][y], "|F7S") && prev != "S" {
		return "N"
	}
	if y < len(maze[x])-1 && runeInSet(maze[x][y+1], "-J7S") && prev != "W" {
		return "E"
	}
	return "S"
}

var dirMap = map[rune]map[string]string{ 
	'F': {"N": "E", "E": "S", "S": "S", "W": "S"},
	'7': {"N": "W", "E": "S", "S": "W", "W": "N"},
	'J': {"N": "N", "E": "N", "S": "W", "W": "N"},
	'L': {"N": "N", "E": "E", "S": "E", "W": "N"},
}

func updateDir(maze rune, dir string) string {
	if newDir, ok := dirMap[maze][dir]; ok {
		return newDir
	}
	return dir
}

func startChr(in string, out string) rune {
	if (in == "S"  && out == "S") || (in =="N" && out == "N") || // was really |
		(in == "N"  && out == "E") || (in =="W" && out == "S") || // was really F
		(in == "N"  && out == "W") || (in =="E" && out == "S"){ // was really 7 
		return 'X'
	}

	return 'Y'
}

func pipe(mazeP *[][]rune, start [2]int) int {
	maze := *mazeP
	length := 0
	x := start[0]
	y := start[1]
	startDir := initDir(maze, x, y, "")
	dir := startDir
	for length == 0 || !runeInSet(maze[x][y], "SXY"){
		if runeInSet(maze[x][y], "|F7") {
			maze[x][y] = 'X' // interior change
		} else{
			maze[x][y] = 'Y' // no interior change
		}
		switch dir {
		case "N":
			x--
		case "S":
			x++
		case "E":
			y++
		case "W":
			y--
		}
		dir = updateDir(maze[x][y], dir)
		length++
	}
	maze[start[0]][start[1]] = startChr(dir, startDir)
	return length
}

func interior(maze [][]rune) int {
	area := 0
	for _, row := range maze {
		inside := false
		for _, m := range row {
			if m == 'X' {
				inside = !inside
			} else if inside && m != 'Y' {
				area++
			}
		}
	}
	return area
}

func main() {
	// file := "test.dat"
	file := "2023-12-10.dat"

	maze, start, err := parse(file)
	if err != nil {
		log.Fatal(err)
	}
	length := pipe(&maze, start)
	fmt.Println("part 1:", length/2)
	fmt.Println("part 2:", interior(maze))
}
