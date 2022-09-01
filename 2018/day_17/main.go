package main

import (
	"fmt"
	"image"
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

type clay struct {
	point int
	ran   [2]int
	xFlag bool
}

func parse(array []string) []clay {
	cs := []clay{}
	for _, v := range array {
		c := clay{}
		if v[0] == 'x' {
			c.xFlag = true
			fmt.Sscanf(v, "x=%d, y=%d..%d", &c.point, &c.ran[0], &c.ran[1])
		} else {
			fmt.Sscanf(v, "y=%d, x=%d..%d", &c.point, &c.ran[0], &c.ran[1])
		}
		cs = append(cs, c)
	}
	return cs
}

const (
	EMPTY byte = iota
	WATER
	CLAY
	FLOW
)

// Not the prettiest...
func createGrid(array []string) [][]byte {
	cs := parse(array)
	var maxX, maxY int
	for _, c := range cs {
		if c.xFlag {
			if c.point > maxX {
				maxX = c.point
			} else if c.ran[1] > maxY {
				maxY = c.ran[1]
			}
		} else {
			if c.point > maxY {
				maxY = c.point
			} else if c.ran[1] > maxX {
				maxX = c.ran[1]
			}
		}
	}
	maxX += 2
	maxY += 1

	grid := make([][]byte, maxY)
	for y := range grid {
		grid[y] = make([]byte, maxX)
	}

	for _, c := range cs {
		if c.xFlag {
			for y := c.ran[0]; y <= c.ran[1]; y++ {
				grid[y][c.point] = CLAY
			}
		} else {
			for x := c.ran[0]; x <= c.ran[1]; x++ {
				grid[c.point][x] = CLAY
			}
		}
	}
	return grid
}

func well(grid *[][]byte, wx int, wy int) (bool, int, int) {
	// Find left wall
	lw := -1
	rw := -1
	for x := wx - 1; x >= 0; x-- {
		if (*grid)[wy][x] == CLAY {
			lw = x
			break
		}
	}
	if lw == -1 {
		return false, 0, 0
	}

	for x := wx + 1; x < len((*grid)[0]); x++ {
		if (*grid)[wy][x] == CLAY {
			rw = x
			break
		}
	}
	if rw == -1 {
		return false, 0, 0
	}

	for x := lw; x <= rw; x++ {
		sv := (*grid)[wy+1][x]
		if sv == EMPTY || sv == FLOW {
			return false, 0, 0
		}
	}
	return true, lw, rw
}

func step(grid *[][]byte, wx int, wy int, seen map[image.Point]bool, maxY int) int {
	sum := 0

	for ; ; wy++ {
		if wy >= maxY {
			return 0
		}
		if seen[image.Pt(wx, wy)] {
			return 0
		}
		seen[image.Pt(wx, wy)] = true

		if (*grid)[wy][wx] != FLOW || (*grid)[wy+1][wx] != FLOW {
			break
		}
	}

	sv := (*grid)[wy][wx]
	if sv == WATER || sv == CLAY {
		return 0
	} else if wy+1 < len(*grid) && ((*grid)[wy+1][wx] == CLAY || (*grid)[wy+1][wx] == WATER) {
		if ok, l, r := well(grid, wx, wy); ok {
			// Start filling the well!!
			for ok {
				for x := l + 1; x < r; x++ {
					seen[image.Pt(x, wy)] = true
					(*grid)[wy][x] = WATER
					sum += 1
				}
				wy -= 1
				ok, l, r = well(grid, wx, wy)
			}
			return sum
		} else {
			if (*grid)[wy][wx] == EMPTY {
				(*grid)[wy][wx] = FLOW
				sum += 1
			}
			sum += step(grid, wx-1, wy, seen, maxY) + step(grid, wx+1, wy, seen, maxY)
			return sum
		}
	} else if (*grid)[wy][wx] == EMPTY {
		(*grid)[wy][wx] = FLOW
		sum += 1 + step(grid, wx, wy+1, seen, maxY)
		return sum
	} else if sv == FLOW {
		sum += step(grid, wx, wy+1, seen, maxY)
		return sum
	}
	// unreachable!
	return -1
}

func maxY(grid *[][]byte) int {
	max := 0
	for y := range *grid {
		for x := range (*grid)[y] {
			if (*grid)[y][x] == CLAY {
				max = y
				break
			}
		}
	}
	return max
}

// Count are the square types we are counting, e.g WATER, FLOW
func solve(array []string, count ...byte) int {
	grid := createGrid(array)

	maxY := maxY(&grid)
	for step(&grid, 500, 1, map[image.Point]bool{}, maxY) > 0 {
	}
	sum := 0
	for y := 0; y < maxY; y++ {
		for x := range (grid)[y] {
			sv := (grid)[y][x]
			for _, c := range count {
				if sv == c {
					sum += 1
				}
			}
		}
	}
	return sum
}

func partTwo(array []string) int {
	return solve(array, WATER)
}

func partOne(array []string) int {
	return solve(array, WATER, FLOW)
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
