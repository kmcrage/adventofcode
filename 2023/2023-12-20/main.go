package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strings"
)

type NodeMap map[string]*Node
type Node struct {
	kind   byte
	input  map[string]bool
	output []string
	state  bool
}

type Message struct {
	src   string
	dst   string
	state bool
}

func makeNode() *Node {
	node := Node{}
	node.input = make(map[string]bool, 0)
	node.output = make([]string, 0)
	return &node
}

func (src *Node) copy() *Node {
	dst := makeNode()
	dst.kind = src.kind
	for k, v := range src.input {
		dst.input[k] = v
	}
	dst.output = append(dst.output, src.output...)
	dst.state = src.state
	return dst
}

func (src NodeMap) copy() NodeMap {
	dst := make(NodeMap, 0)
	for k, n := range src {
		dst[k] = n.copy()
	}
	return dst
}

// greatest common divisor (GCD) via Euclidean algorithm
func GCD(a, b int) int {
	for b != 0 {
		a, b = b, a%b
	}
	return a
}

// find Least Common Multiple (LCM) via GCD
func LCM(a, b int, integers ...int) int {
	result := a * b / GCD(a, b)

	for _, i := range integers {
		result = LCM(result, i)
	}

	return result
}

func parse(file string) (NodeMap, error) {
	f, err := os.Open(file)
	if err != nil {
		return nil, err
	}
	defer f.Close()

	scanner := bufio.NewScanner(f)
	nodes := make(NodeMap, 0)
	for scanner.Scan() {
		tokens := strings.Split(scanner.Text(), " -> ")
		outputs := strings.Split(tokens[1], ", ")
		name := tokens[0][1:]
		kind := tokens[0][0]
		if tokens[0] == "broadcaster" {
			name = tokens[0]
			kind = '>'
		}
		if _, ok := nodes[name]; !ok {
			nodes[name] = makeNode()
		}
		nodes[name].kind = kind
		nodes[name].output = outputs

		for _, out := range outputs {
			if _, ok := nodes[out]; !ok {
				nodes[out] = makeNode()
			}
			nodes[out].input[name] = false
		}
	}
	return nodes, nil
}

func (node Node) send(src string, state bool) []Message {
	msgs := make([]Message, 0)
	for _, dst := range node.output {
		msgs = append(msgs, Message{src, dst, state})
	}
	return msgs
}

func activate(nodes NodeMap, msg Message) []Message {
	msgs := make([]Message, 0)
	switch nodes[msg.dst].kind {
	case '>':
		msgs = append(msgs, nodes[msg.dst].send(msg.dst, msg.state)...)
	case '%':
		if !msg.state {
			nodes[msg.dst].state = !nodes[msg.dst].state
			msgs = append(msgs, nodes[msg.dst].send(msg.dst, nodes[msg.dst].state)...)
		}
	case '&':
		nodes[msg.dst].input[msg.src] = msg.state
		state := false
		for _, s := range nodes[msg.dst].input {
			if !s {
				state = true
			}
		}
		msgs = append(msgs, nodes[msg.dst].send(msg.dst, state)...)

	}
	return msgs
}

func iterate(nodes NodeMap, iterate int) int {
	high := 0
	low := 0
	for i := 0; i < iterate; i++ {
		start := Message{src: "button", dst: "broadcaster"}
		queue := []Message{start}

		for len(queue) > 0 {
			msg := queue[0]
			if msg.state {
				high++
			} else {
				low++
			}
			// fmt.Println(msg.src, "-", msg.state, "->", msg.dst)
			queue = append(queue[1:], activate(nodes, msg)...)
		}
		//fmt.Println()
	}
	//fmt.Println(high, low)
	return high * low
}

func minPresses(nodes NodeMap, target string) int {
	for len(nodes[target].input) == 1 {
		for n := range nodes[target].input {
			target = n
		}
	}
	fmt.Println("conjunction:", string(nodes[target].kind), target)

	periods := minPressesTrue(nodes.copy(), nodes[target].input)
	period := 0
	for n, p := range periods {
		fmt.Println("node:", n, "period:", p)
		if period == 0 {
			period = p
		} else {
			period = LCM(period, p)
		}
	}
	return period
}

func minPressesTrue(nodes NodeMap, targets map[string]bool) map[string]int {
	counts := make(map[string]int, len(targets))
	presses := 0
	for {
		presses++
		start := Message{src: "button", dst: "broadcaster"}
		queue := []Message{start}

		for len(queue) > 0 {
			msg := queue[0]
			_, ok := targets[msg.dst]
			if ok && !msg.state && counts[msg.dst] == 0 {
				counts[msg.dst] = presses
				if len(counts) == len(targets) {
					return counts
				}
			}
			queue = append(queue[1:], activate(nodes, msg)...)
		}
	}
}

func main() {
	// file := "test.dat"
	file := "2023-12-20.dat"

	nodes, err := parse(file)
	if err != nil {
		log.Fatal(err)
	}

	fmt.Println("part 1:", iterate(nodes.copy(), 1000))
	fmt.Println("part 2:", minPresses(nodes.copy(), "rx"))
}
