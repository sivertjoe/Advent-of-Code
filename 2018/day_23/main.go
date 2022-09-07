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

type nanobot struct {
	point  point
	radius int
}

func parse(array []string) []nanobot {
	res := []nanobot{}
	for _, line := range array {
		n := nanobot{}
		fmt.Sscanf(line, "pos=<%d,%d,%d>, r=%d", &n.point.coords[0], &n.point.coords[1], &n.point.coords[2], &n.radius)
		res = append(res, n)
	}
	return res
}

func inRange(n nanobot, p point) bool {
	return manhattan(p, n.point) <= n.radius
}

func abs(n int) int {
	if n < 0 {
		return -n
	}
	return n
}

type point struct {
	coords [3]int
}

func manhattan(p1, p2 point) int {
	// Loop unrolling!
	return abs(p2.coords[0]-p1.coords[0]) + abs(p2.coords[1]-p1.coords[1]) + abs(p2.coords[2]-p1.coords[2])

	// 6(!) ms faster than v
	// sum := 0
	// for i := range p1.coords {
	// 	sum += abs(p2.coords[i] - p1.coords[i])
	// }
	// return sum
}

func partOne(array []string) int {
	nbs := parse(array)
	max := nbs[0]
	for _, n := range nbs {
		if n.radius > max.radius {
			max = n
		}
	}

	count := 0
	for _, n := range nbs {
		if inRange(max, n.point) {
			count += 1
		}
	}

	return count
}

func minMax(ns []nanobot) (point, point) {
	var min, max point
	min = ns[0].point

	for _, b := range ns {
		for i := 0; i < 3; i++ {
			if b.point.coords[i] < min.coords[i] {
				min.coords[i] = b.point.coords[i]
			}
			if b.point.coords[i] > max.coords[i] {
				max.coords[i] = b.point.coords[i]
			}
		}
	}

	return min, max
}

func count(ns *[]nanobot, pt point) int {
	sum := 0
	for i := 0; i < len(*ns); i++ {
		if inRange((*ns)[i], pt) {
			sum += 1
		}
	}
	return sum
}

func update(min, max *point, best point, width int) {
	for i := 0; i < 3; i++ {
		(*min).coords[i] = best.coords[i] - width
		(*max).coords[i] = best.coords[i] + width
	}
}

func partTwo(array []string) int {
	ns := parse(array)

	// defaults to 0 , 0, 0
	origin := point{}

	min, max := minMax(ns)

	best := point{}
	for width := max.coords[0] - min.coords[0]; width > 0; width /= 2 {
		maxCount := 0
		for x := min.coords[0]; x < max.coords[0]+1; x += width {
			for y := min.coords[1]; y < max.coords[1]+1; y += width {
				for z := min.coords[2]; z < max.coords[2]+1; z += width {
					pt := point{[3]int{x, y, z}}
					count := count(&ns, pt)

					// The point has the same amout of reachable points, but is closest to the origin
					closerPoint := (maxCount == count && manhattan(best, origin) > manhattan(pt, origin))

					if count > maxCount || closerPoint {
						maxCount = count
						best = pt
					}
				}
			}
		}
		update(&min, &max, best, width)
	}

	return manhattan(best, origin)
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
