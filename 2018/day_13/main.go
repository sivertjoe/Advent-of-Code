package main

import (
	"fmt"
	"image"
	"os"
	"sort"
	"strings"
	"time"
)

func readLine(path string) []string {
	b, _ := os.ReadFile(path)
	str := string(b)
	return strings.Split(str, "\n")
}

type cart struct {
	pos   image.Point
	dir   byte
	count int
}

func mod(a, b int) int {
	return (a%b + b) % b
}

func s(cart1 cart, cart2 cart) bool {
	if cart1.pos.Y < cart2.pos.Y {
		return true
	} else if cart1.pos.Y > cart2.pos.Y {
		return false
	} else {
		return cart1.pos.X < cart2.pos.X
	}
}

func direction(dir byte) (int, int) {
	switch dir {
	case '^':
		return 0, -1
	case '>':
		return 1, 0
	case 'v':
		return 0, 1

	case '<':
		return -1, 0
	}

	// unreachable
	return 0, 0
}

// Declare the variables as globals to shave 5 ms off the time :^)
var sym map[byte]int = nil
var back map[int]byte = nil
var inc map[int]int = nil

// Yes. Very readable, indeed.
func intersection(cart cart) byte {
	if sym == nil {
		sym = map[byte]int{'^': 0, '>': 1, 'v': 2, '<': 3}
		back = map[int]byte{0: '^', 1: '>', 2: 'v', 3: '<'}
		inc = map[int]int{0: -1, 1: 0, 2: 1}
	}

	return back[mod(sym[cart.dir]+inc[mod(cart.count, 3)], 4)]
}

var table map[byte]map[byte]byte = nil

func turn(cart cart, sym byte) byte {
	if table == nil {
		table = map[byte]map[byte]byte{
			'/':  {'v': '<', '>': '^', '^': '>', '<': 'v'},
			'\\': {'v': '>', '^': '<', '<': '^', '>': 'v'}}
	}
	return table[sym][cart.dir]
}

func step(array *[]string, carts map[image.Point]cart, point image.Point) (bool, int, int) {
	// Cart might have crashed, so we need to check if its still in the map
	cart, ok := carts[point]
	if !ok {
		return false, 0, 0
	}

	delete(carts, cart.pos)

	x, y := direction(cart.dir)
	cart.pos.X += x
	cart.pos.Y += y

	if _, ok := carts[cart.pos]; ok {
		delete(carts, cart.pos)
		return true, cart.pos.X, cart.pos.Y
	}

	if len(carts) == 0 {
		return true, cart.pos.X, cart.pos.Y
	}

	sv := (*array)[cart.pos.Y][cart.pos.X]
	switch sv {
	case '+':
		cart.dir = intersection(cart)
		cart.count += 1
	case '\\', '/':
		cart.dir = turn(cart, sv)
	}

	carts[cart.pos] = cart

	return false, 0, 0
}

func solve(array []string, task Task) string {
	carts := map[image.Point]cart{}

	for y := range array {
		for x := range array[y] {
			v := array[y][x]
			if v == '>' || v == '<' || v == '^' || v == 'v' {
				p := image.Pt(x, y)
				carts[p] = cart{pos: p, dir: v}
			}
		}
	}

	for {
		cartArray := []cart{}
		for _, v := range carts {
			cartArray = append(cartArray, v)
		}

		sort.Slice(cartArray, func(i, j int) bool { return s(cartArray[i], cartArray[j]) })

		for _, cart := range cartArray {
			col, x, y := step(&array, carts, cart.pos)

			if col && (task == Silver || len(carts) == 0) {
				return fmt.Sprintf("%d,%d", x, y)
			}
		}
	}

}

func partOne(array []string) string {
	return solve(array, Silver)
}

func partTwo(array []string) string {
	return solve(array, Gold)
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
