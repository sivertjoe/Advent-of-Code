package main

import (
	"fmt"
	"image"
	"os"
	"time"
)

func readLine(path string) string {
	b, _ := os.ReadFile(path)
	str := string(b)
	return str
}

func pop(s []image.Point) ([]image.Point, image.Point) {
	l := len(s)
	return s[:l-1], s[l-1]
}

func peek(s []image.Point) image.Point {
	l := len(s)
	return s[l-1]
}

func parse(line string) map[image.Point]int {
	move := map[byte]image.Point{'N': image.Pt(0, -1), 'S': image.Pt(0, 1), 'W': image.Pt(-1, 0), 'E': image.Pt(1, 0)}
	stack := []image.Point{}
	current := image.Pt(0, 0)
	rooms := map[image.Point]int{current: 0}

	for _, v := range line {
		switch v {
		case '(':
			stack = append(stack, current)
		case ')':
			stack, current = pop(stack)
		case '|':
			current = peek(stack)
		case 'N', 'S', 'W', 'E':
			next := rooms[current] + 1
			inc := move[byte(v)]
			current.X += inc.X
			current.Y += inc.Y

			if curr, ok := rooms[current]; !(ok && curr < next) {
				rooms[current] = next
			}
		}
	}

	return rooms
}

func partOne(line string) int {
	rooms := parse(line)
	max := 0
	for _, v := range rooms {
		if v > max {
			max = v
		}
	}

	return max
}

func partTwo(line string) int {
	rooms := parse(line)
	count := 0
	for _, v := range rooms {
		if v >= 1000 {
			count += 1
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
	timeFunc(Gold, partTwo, array)
}
