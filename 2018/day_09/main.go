package main

import (
	"fmt"
	"os"
	"strings"
	"time"
)

func readLine(path string) string {
	b, _ := os.ReadFile(path)
	str := string(b)
	return strings.Split(str, "\n")[0]
}

type node struct {
	left  int
	right int
	value int
}

func insert(a []int, c int, i int) []int {
	return append(a[:i], append([]int{c}, a[i:]...)...)
}

func remove(slice []node, idx int) []node {
	node := slice[idx]

	slice[node.left].right = node.right
	slice[node.right].left = node.left

	return slice
}

func normalInsert(array []node, idx int, val int) (int, []node) {
	if val == 1 {
		array[1].value = 1
		array[0].right = 1
		array[0].left = 1
		return 1, array
	} else {
		array[val].value = val

		in := array[array[idx].right].right
		array[val].left = array[in].left
		array[val].right = in
		array[array[in].left].right = val
		array[in].left = val

		return val, array
	}
}

func run(line string, modifier int) int {
	var numPlayers int
	var lastMarble int

	fmt.Sscanf(line, "%d players; last marble is worth %d points", &numPlayers, &lastMarble)
	lastMarble *= modifier

	playerScores := make([]int, numPlayers)
	current := 1
	idx := 0
	board := make([]node, lastMarble+1)
	round := 0

	for {
		if current > lastMarble {
			break
		}

		if current%23 == 0 {
			currentPlayer := round % numPlayers
			playerScores[currentPlayer] += current

			for i := 0; i < 7; i++ {
				idx = board[idx].left
			}

			playerScores[currentPlayer] += board[idx].value
			board = remove(board, idx)
			idx = board[idx].right
		} else {
			idx, board = normalInsert(board, idx, current)
		}
		round++
		current++
	}

	max := 0
	for _, v := range playerScores {
		if v > max {
			max = v
		}
	}

	return max
}

func partOne(line string) int {
	return run(line, 1)
}

func partTwo(line string) int {
	return run(line, 100)
}

type Task int

const (
	Silver Task = 0
	Gold        = 1
)

func timeFunc[T any, S any](task Task, f func(T) S, arg T) {

	t0 := time.Now()
	res := f(arg)
	elapsed := time.Now().Sub(t0).Milliseconds()

	switch task {
	case Silver:
		fmt.Printf("(%dms)\tTask one: \x1b[0;34;34m%v\x1b[0m\n", elapsed, res)
	case Gold:
		fmt.Printf("(%dms)\tTask two: \x1b[0;33;10m%v\x1b[0m\n", elapsed, res)
	}
}

func main() {
	args := os.Args
	if len(args) != 2 {
		fmt.Println("Usage: ./main <input file>")
		os.Exit(-1)
	}

	array := readLine(args[1])
	timeFunc(Silver, partOne, array)
	timeFunc(Gold, partTwo, array)
}
