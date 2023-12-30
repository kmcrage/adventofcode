package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"

	"gonum.org/v1/gonum/mat"
)

type Hail struct {
	pos *mat.VecDense
	vel *mat.VecDense
}

func pos(s string) *mat.VecDense {
	tokens := strings.Split(s, ",")
	data := make([]float64, 3)
	for i := 0; i < 3; i++ {
		data[i], _ = strconv.ParseFloat(tokens[i], 64)
	}
	return mat.NewVecDense(3, data)
}

func parse(file string) ([]Hail, error) {
	f, err := os.Open(file)
	if err != nil {
		return nil, err
	}
	defer f.Close()

	scanner := bufio.NewScanner(f)
	hails := make([]Hail, 0)
	for scanner.Scan() {
		tokens := strings.Split(strings.ReplaceAll(scanner.Text(), " ", ""), "@")
		hails = append(hails, Hail{pos(tokens[0]), pos(tokens[1])})
	}
	return hails, nil
}

func intersections(hails []Hail, mn, mx float64) int {
	colls := 0
	for i, haili := range hails {
		for _, hailj := range hails[0:i] {
			pos := mat.NewVecDense(2,
				[]float64{haili.pos.AtVec(0) - hailj.pos.AtVec(0),
					haili.pos.AtVec(1) - hailj.pos.AtVec(1)})
			mtx := mat.NewDense(2, 2,
				[]float64{
					-haili.vel.AtVec(0), hailj.vel.AtVec(0),
					-haili.vel.AtVec(1), hailj.vel.AtVec(1)})
			times := mat.NewDense(2, 1, nil)
			err := times.Solve(mtx, pos)
			if err == nil {
				if times.At(0, 0) > 0 && times.At(1, 0) > 0 {
					intersect := mat.NewVecDense(3, nil)
					intersect.AddScaledVec(haili.pos, times.At(0, 0), haili.vel)
					if mn <= intersect.AtVec(0) && intersect.AtVec(0) <= mx &&
						mn <= intersect.AtVec(1) && intersect.AtVec(1) <= mx {
						colls++
					}
				}
			}
		}
	}
	return colls
}

func abs(n int) int {
	if n < 0 {
		return -n
	} else {
		return n
	}
}

func possibleVel(diffPosF, velF float64, maybe map[int]bool) map[int]bool {
	diffPos := int(diffPosF)
	vel := int(velF)
	match := make(map[int]bool, len(maybe))
	for v := range maybe {
		if vel != v && abs(diffPos)% abs(vel-v) == 0 {
			match[v] = true
		} else if vel == v && diffPos == 0 {
			match[v] = true
		}
	}
	return match
}

func solveVelocity(hails []Hail) *mat.VecDense {
	var maybeVel [3]map[int]bool
	for i := 0; i < 3; i++ {
		maybeVel[i] = make(map[int]bool, 2000)
		for v := -1000; v < 1000; v++ {
			maybeVel[i][v] = true
		}
	}

	data := make([]float64, 3)
	for i, haili := range hails {
		for _, hailj := range hails[i:] {
			for k := 0; k < 3; k++ {
				if haili.vel.AtVec(k) == hailj.vel.AtVec(k) {
					diffPos := mat.NewVecDense(3, nil)
					diffPos.AddScaledVec(haili.pos, -1, hailj.pos)
					maybeVel[k] = possibleVel(diffPos.AtVec(k), haili.vel.AtVec(k), maybeVel[k])
				}
			}
			if len(maybeVel[0]) == 1 && len(maybeVel[1]) == 1 && len(maybeVel[2]) == 1 {
				for k := 0; k < 3; k++ {
					for v := range maybeVel[k] {
						data[k] = float64(v)
					}
				}
				return mat.NewVecDense(3, data)
			}
		}
	}
	return mat.NewVecDense(3, data)
}

func collisions(hails []Hail) int {
	vel := solveVelocity(hails)

	hail0v := mat.NewVecDense(3, nil)
	hail0v.AddScaledVec(vel, -1, hails[0].vel)
	hail1v := mat.NewVecDense(3, nil)
	hail1v.AddScaledVec(vel, -1, hails[1].vel)

	pos := mat.NewVecDense(2,
		[]float64{hails[0].pos.AtVec(0) - hails[1].pos.AtVec(0),
			hails[0].pos.AtVec(1) - hails[1].pos.AtVec(1)})
	mtx := mat.NewDense(2, 2,
		[]float64{
			hail0v.AtVec(0), hail1v.AtVec(0),
			hail0v.AtVec(1), hail1v.AtVec(1)})
	times := mat.NewDense(2, 1, nil)
	err := times.Solve(mtx, pos)
	if err == nil {
		intersect := mat.NewVecDense(3, nil)
		intersect.AddScaledVec(hails[0].pos, -times.At(0, 0), hail0v)
		return int(intersect.AtVec(0) + intersect.AtVec(1) + intersect.AtVec(2) + 0.5)
	}
	return 0
}



func main() {
	// file := "test.dat"
	file := "2023-12-24.dat"

	hails, err := parse(file)
	if err != nil {
		log.Fatal(err)
	}
	fmt.Println("part 1:", intersections(hails, 200000000000000, 400000000000000))
	fmt.Println("part 2:", collisions(hails))
}
