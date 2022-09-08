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
	s := strings.Split(str, "\n")
	return s[:len(s)-1]
}

type point struct {
	coord [4]int
}

func parse(array []string) []point {
	ps := []point{}
	for _, v := range array {
		p := point{}
		fmt.Sscanf(v, "%d,%d,%d,%d", &p.coord[0], &p.coord[1], &p.coord[2], &p.coord[3])
		ps = append(ps, p)
	}
	return ps
}

func abs(n int) int {
	if n < 0 {
		return -n
	}
	return n
}
func manhattan(p1, p2 point) int {
	sum := 0
	for i := range p1.coord {
		sum += abs(p1.coord[i] - p2.coord[i])
	}
	return sum
}

func partOne(array []string) int {
	ps := parse(array)
	seen := map[point]bool{}

	inRange := map[point][]point{}

	for _, p := range ps {
		for _, p2 := range ps {
			if p != p2 {
				d := manhattan(p, p2)
				if d <= 3 {
					inRange[p] = append(inRange[p], p2)
				}
			}
		}
	}

	count := 0
	for _, p := range ps {
		if !seen[p] {
			count += 1

			curr := []point{p}
			for len(curr) > 0 {
				new := []point{}
				for _, v := range curr {
					if seen[v] {
						continue
					}
					seen[v] = true
					for _, y := range inRange[v] {
						new = append(new, y)
					}
				}
				curr = new
			}
		}
	}
	return count
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
}
