package main

import (
	"fmt"
	"os"
	"strings"
)

type ins struct {
	fst string
	snd string
}

func readLine(path string) []string {
	b, _ := os.ReadFile(path)
	str := string(b)
	arr := strings.Split(str, "\n")
	return arr[:len(arr)-1]
}

func parse(line string) ins {
	var fst, snd string
	fmt.Sscanf(line, "Step %s must be finished before step %s can begin.", &fst, &snd)
	return ins{fst: fst, snd: snd}
}

func contains[T comparable](t T, ts []T) bool {
	for _, v := range ts {
		if t == v {
			return true
		}
	}
	return false
}

func pickSmallest(array []string) (string, []string) {
	min := array[0]
	idx := 0
	for i, ch := range array {
		if ch < min {
			min = ch
			idx = i
		}
	}

	if len(array) == 1 {
		return min, []string{}
	} else {
		return min, append(array[:idx], array[idx+1:]...)
	}
}

func canPick(ch string, ins []ins, done []string) bool {
	for _, in := range ins {
		if in.snd == ch && !contains(in.fst, done) {
			return false
		}
	}
	return true
}

func appendPool(ch string, pool []string, ins []ins, done []string) []string {
	for _, c := range ins {
		if c.fst == ch && !contains(c.snd, pool) && canPick(c.snd, ins, done) {
			pool = append(pool, c.snd)
		}
	}
	return pool
}

func partOne(array []string) string {

	order := []ins{}

	for _, line := range array {
		rule := parse(line)
		order = append(order, rule)
	}

	pool := []string{}
	for _, in := range order {
		consider := in.fst
		flag := true
		for _, v := range order {
			if v.snd == consider {
				flag = false
				break
			}
		}

		if flag && !contains(consider, pool) {
			pool = append(pool, consider)
		}
	}

	ans := ""
	var ch string
	done := []string{}

	for len(pool) > 0 {
		ch, pool = pickSmallest(pool)
		done = append(done, ch)

		ans += ch
		pool = appendPool(ch, pool, order, done)
	}

	return ans
}

func partTwo(array []string) int {
	return solve(array, 5, 60)
}

func solve(array []string, numWorkers int, baseTime int) int {

	order := []ins{}

	for _, line := range array {
		rule := parse(line)
		order = append(order, rule)
	}

	pool := []string{}
	for _, in := range order {
		consider := in.fst
		flag := true
		for _, v := range order {
			if v.snd == consider {
				flag = false
				break
			}
		}

		if flag && !contains(consider, pool) {
			pool = append(pool, consider)
		}
	}

	workers := make([]int, numWorkers)
	workersString := make([]string, numWorkers)

	time := 0
	var ch string
	done := []string{}

	m := map[string]string{}
	for _, in := range order {
		m[in.fst] = in.fst
		m[in.snd] = in.snd
	}

	stopLen := len(m)

	for len(done) < stopLen {
		for i := 0; i < numWorkers; i++ {
			workers[i]--

			if workers[i] <= 0 && workersString[i] != "" {
				done = append(done, workersString[i])
				pool = appendPool(workersString[i], pool, order, done)
				workersString[i] = ""
			}

			if workers[i] <= 0 && len(pool) > 0 {
				ch, pool = pickSmallest(pool)
				workersString[i] = ch
				workers[i] = baseTime + int(ch[0]-'A') + 1
			}
		}

		time += 1
	}

	return time - 1
}

func main() {
	args := os.Args
	if len(args) != 2 {
		fmt.Println("Usage: ./main <input file>")
		os.Exit(-1)
	}

	array := readLine(args[1])
	fmt.Println(partOne(array))
	fmt.Println(partTwo(array))
}
