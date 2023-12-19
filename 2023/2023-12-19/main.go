package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"regexp"
	"strconv"
	"strings"
)

type Part map[byte]int
type WorkflowMap map[string][]string

type State struct {
	min      Part
	max      Part
	workflow string
}

var workflowRe = regexp.MustCompile(`(\S+)\{(\S+)\}`)
var partsRe = regexp.MustCompile(`x=(\d+),m=(\d+),a=(\d+),s=(\d+)`)

func parse(file string) (WorkflowMap, []Part, error) {
	f, err := os.Open(file)
	if err != nil {
		return nil, nil, err
	}
	defer f.Close()

	scanner := bufio.NewScanner(f)
	workflows := make(WorkflowMap, 0)
	for scanner.Scan() {
		match := workflowRe.FindStringSubmatch(scanner.Text())
		if match == nil {
			break
		}
		workflows[match[1]] = strings.Split(match[2], ",")
	}
	parts := make([]Part, 0)
	for scanner.Scan() {
		match := partsRe.FindStringSubmatch(scanner.Text())
		x, _ := strconv.Atoi(match[1])
		m, _ := strconv.Atoi(match[2])
		a, _ := strconv.Atoi(match[3])
		s, _ := strconv.Atoi(match[4])
		parts = append(parts, Part{'x': x, 'm': m, 'a': a, 's': s})
	}

	return workflows, parts, nil
}

func (part Part)apply(rule string) string {
	tokens := strings.Split(rule, ":")
	if len(tokens) == 1 {
		return tokens[0]
	}
	arg, _ := strconv.Atoi(tokens[0][2:])
	eval := false
	switch rule[1] {
	case '<':
		eval = part[rule[0]] < arg
	case '>':
		eval = part[rule[0]] > arg
	}

	result := ""
	if eval {
		result = tokens[1]
	}
	return result
}

func (state State)copy() State {
	dup := State{make(Part, 4), make(Part, 4), state.workflow}
	for k, v := range state.min {
		dup.min[k] = v
	}
	for k, v := range state.max {
		dup.max[k] = v
	}

	return dup
}

func (state State)apply(rule string) (State, State) {
	passed := state.copy()
	accepted := state.copy()

	tokens := strings.Split(rule, ":")
	if len(tokens) == 1 {
		passed.workflow = "R"
		accepted.workflow = tokens[0]
		return passed, accepted
	}

	accepted.workflow = tokens[1]
	arg, _ := strconv.Atoi(tokens[0][2:])

	switch rule[1] {
	case '<':
		accepted.max[rule[0]] = min(accepted.max[rule[0]], arg-1)
		passed.min[rule[0]] = max(passed.min[rule[0]], arg)
	case '>':
		accepted.min[rule[0]] = max(accepted.min[rule[0]], arg+1)
		passed.max[rule[0]] = min(passed.max[rule[0]], arg)
	}

	if accepted.min[rule[0]] > accepted.max[rule[0]] {
		accepted.workflow = "R"
	}
	if passed.min[rule[0]] > passed.max[rule[0]] {
		passed.workflow = "R"
	}

	return passed, accepted
}

func (state State)combinations() int {
	product := 1
	for k := range state.max {
		product *= state.max[k] - state.min[k] + 1
	}
	return product
}

func combinations(workflows WorkflowMap, mn int, mx int) int {
	combinations := 0
	start := State{
		Part{'x': mn, 'm': mn, 'a': mn, 's': mn},
		Part{'x': mx, 'm': mx, 'a': mx, 's': mx},
		"in",
	}
	queue := []State{start}

	var next State
	var state State
	for len(queue) > 0 {
		state = queue[0]
		queue = queue[1:]
		for _, rule := range workflows[state.workflow] {
			state, next = state.apply(rule)
			switch next.workflow {
			case "A":
				combinations += next.combinations()
			case "R":
			default:
				queue = append(queue, next)
			}
			if state.workflow == "R" {
				break
			}
		}
	}
	return combinations
}

func accepted(workflows WorkflowMap, parts []Part) int {
	accepted := 0
	for _, part := range parts {
		workflow := "in"
		for workflow != "A" && workflow != "R" {
			for _, rule := range workflows[workflow] {
				result := part.apply(rule)
				if result != "" {
					workflow = result
					break
				}
			}
		}
		if workflow == "A" {
			for _, val := range part {
				accepted += val
			}
		}
	}
	return accepted
}

func main() {
	// file := "test.dat"
	file := "2023-12-19.dat"

	workflows, parts, err := parse(file)
	if err != nil {
		log.Fatal(err)
	}
	fmt.Println("part 1:", accepted(workflows, parts))
	fmt.Println("part 2:", combinations(workflows, 1, 4000))
}
