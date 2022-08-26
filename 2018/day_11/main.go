package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"
	"time"
)

func powerLevel(x int, y int, serial int) int {
	rackId := x + 10
	res := ((rackId * y) + serial) * rackId
	res = res % 1000 / 100
	return res - 5
}

func partOne(serial int) string {
	grid := [300][300]int{}
	for y := 0; y < 300; y++ {
		for x := 0; x < 300; x++ {
			grid[y][x] = powerLevel(x+1, y+1, serial)
		}
	}

	gridSize := 3
	max := 0
	var xi, yi int
	for y := 0; y < 300-gridSize; y++ {
		for x := 0; x < 300-gridSize; x++ {
			sum := 0

			for yy := y; yy < y+gridSize; yy++ {
				for xx := x; xx < x+gridSize; xx++ {
					sum += grid[yy][xx]
				}
			}

			if sum > max {
				max = sum
				xi = x
				yi = y
			}
		}
	}
	return fmt.Sprintf("%d,%d", xi+1, yi+1)
}

func calcGrid(grid *[300][300]int, x int, y int, serial int, len int) int {
	sum := 0
	for yy := y; yy < y+len; yy++ {
		for xx := x; xx < x+len; xx++ {
			sum += grid[yy][xx]
		}
	}
	return sum
}

func partTwo(serial int) string {
	grid := [300][300]int{}
	for y := 0; y < 300; y++ {
		for x := 0; x < 300; x++ {
			grid[y][x] = powerLevel(x+1, y+1, serial)
		}
	}

	max := 0
	var xi, yi int
	var maxS int
	for y := 0; y < 300; y++ {
		for x := 0; x < 300; x++ {

			const cap = 20
			for s := 3; y+s < 300 && x+s < 300 && s < cap; s++ {
				newSum := calcGrid(&grid, x, y, serial, s)

				if newSum >= max {
					max = newSum
					xi = x
					yi = y
					maxS = s
				}
			}
		}
	}
	return fmt.Sprintf("%d,%d,%d", xi+1, yi+1, maxS)
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

	b, err := os.ReadFile(args[1])
	if err != nil {
		fmt.Println(err)
		os.Exit(0)
	}
	serial, err := strconv.Atoi(strings.Trim(string(b), "\n"))
	if err != nil {
		fmt.Println(err)
		os.Exit(0)
	}

	timeFunc(Silver, partOne, serial)
	timeFunc(Gold, partTwo, serial)
}
