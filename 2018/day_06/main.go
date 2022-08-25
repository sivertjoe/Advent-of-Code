package main

import (
	"fmt"
	"os"
	"strings"
)

func readLine(path string) []string {
	b, _ := os.ReadFile(path)
	str := string(b)
	arr := strings.Split(str, "\n")
	return arr[:len(arr)-1]
}

func abs(i int) int {
	if i < 0 {
		return -i
	}
	return i
}

func manhattanDistance(p1 point, p2 point) int {
	return abs(p1.x-p2.x) + abs(p1.y-p2.y)
}

func contains(p point, points []point) bool {
	for _, val := range points {
		if p.x == val.x && p.y == val.y {
			return true
		}
	}
	return false
}

func findClosest(point point, points []point) int {
	const MaxUint = ^uint(0)
	const MaxInt = int(MaxUint >> 1)

	smallestId := -1
	smallest := MaxInt
	flag := false

	for i, p := range points {
		dist := manhattanDistance(point, p)
		if dist < smallest {
			smallest = dist
			smallestId = i
			flag = false
		} else if dist == smallest {
			flag = !contains(p, points)
		}
	}
	// return -1 if the point is equally close to two (or more) points
	if flag {
		return -1
	}
	return smallestId

}

func partOne(array []string) int {
	var points []point
	bottomRight := point{x: 0, y: 0}

	for _, line := range array {
		point := parse(line)
		points = append(points, point)
		if point.x > bottomRight.x {
			bottomRight.x = point.x
		}
		if point.y > bottomRight.y {
			bottomRight.y = point.y
		}
	}

	grid := make([][]int, bottomRight.y)
	for i := range grid {
		grid[i] = make([]int, bottomRight.x)
		for x := 0; x < bottomRight.x; x++ {
			grid[i][x] = -2
		}
	}

	for y := 0; y < bottomRight.y; y++ {
		for x := 0; x < bottomRight.x; x++ {
			p := point{x: x, y: y}
			grid[y][x] = findClosest(p, points)
		}
	}

	notInfs := []int{}
	for i := 0; i < len(points); i++ {
		notInfs = append(notInfs, i)
	}

	for x := 0; x < bottomRight.x; x++ {
		v1 := grid[0][x]
		v2 := grid[bottomRight.y-1][x]

		if v1 >= 0 {
			notInfs[v1] = -1
		}
		if v2 >= 0 {
			notInfs[v2] = -1
		}

	}

	for y := 0; y < bottomRight.y; y++ {
		v1 := grid[y][0]
		v2 := grid[y][bottomRight.x-1]

		if v1 >= 0 {
			notInfs[v1] = -1
		}
		if v2 >= 0 {
			notInfs[v2] = -1
		}
	}

	count := func(v int) int {
		sum := 0
		for y := 0; y < bottomRight.y; y++ {
			for x := 0; x < bottomRight.x; x++ {
				if v == grid[y][x] {
					sum += 1
				}
			}
		}
		return sum
	}
	biggest := 0
	for _, v := range notInfs {
		if v > 0 {
			sum := count(v)
			if sum > biggest {
				biggest = sum
			}
		}
	}

	return biggest
}

func sumManhattan(p point, ps []point) int {
	sum := 0
	for _, point := range ps {
		sum += manhattanDistance(p, point)
	}
	return sum
}

func partTwo(array []string) int {
	var points []point
	bottomRight := point{x: 0, y: 0}

	for _, line := range array {
		point := parse(line)
		points = append(points, point)
		if point.x > bottomRight.x {
			bottomRight.x = point.x
		}
		if point.y > bottomRight.y {
			bottomRight.y = point.y
		}
	}

	const sumLimit = 10000

	sum := 0
	for y := 0; y < bottomRight.y; y++ {
		for x := 0; x < bottomRight.x; x++ {
			p := point{x: x, y: y}
			if sumManhattan(p, points) < sumLimit {
				sum += 1
			}
		}
	}

	return sum

}

func parse(line string) point {
	var x, y int
	fmt.Sscanf(line, "%d, %d", &x, &y)

	return point{x: x, y: y}
}

type point struct {
	x int
	y int
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
