package main

import (
	"fmt"
	"os"
	"strings"
	"time"
)

func readLine(path string) []string {
	b, _ := os.ReadFile(path)
	str := string(b)
	a := strings.Split(str, "\n")

	return a
}

func createState(array []string) (string, map[string]rune) {
	var init string
	fmt.Sscanf(array[0], "initial state: %s", &init)
	rules := map[string]rune{}

	for _, line := range array[2:] {
		var rule string
		var out rune
		fmt.Sscanf(line, "%s => %c", &rule, &out)
		rules[rule] = out

	}

	return init, rules
}

func minMax(array []rune) (int, int) {
	min := len(array)
	max := 0
	for i := 0; i < len(array); i++ {
		if array[i] == '#' {
			max = i
			if i < min {
				min = i
			}
		}
	}

	return min, max
}

func converged(old *[]rune, new *[]rune) bool {
	minOld, maxOld := minMax(*old)
	minNew, _ := minMax(*new)

	for i := 0; i < maxOld-minOld; i++ {
		if (*old)[minOld+i] != (*new)[minNew+i] {
			return false
		}
	}
	return true
}

func sum(state []rune, zero int) int {
	sum := 0
	for i, r := range state {
		if r == '#' {
			sum += i - zero
		}
	}
	return sum
}

func partOne(array []string) int {
	return solve(array, 20)
}

func solve(array []string, limit int) int {
	state, rules := createState(array)

	for gen := 1; ; gen++ {
		state = "....." + state + "....."

		new := []rune(state)
		for i := 2; i < len(state)-2; i++ {
			new[i] = rules[state[i-2:i+3]]
		}

		if gen == limit {
			return sum(new, gen*5)
		}

		nn := []rune(state)

		// The pattern converges when it reaches a stable pattern that shifts
		// by some amount.
		if converged(&nn, &new) {
			zero := (gen + gen*5) - limit
			return sum(nn, zero)
		}

		state = string(new)
	}
}

func partTwo(array []string) int {
	return solve(array, 50000000000)
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
