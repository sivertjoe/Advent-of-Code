package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"
	"time"
)

func readLine(path string) []string {
	b, _ := os.ReadFile(path)
	str := string(b)
	return strings.Split(str, "\n")
}

func split(n uint8) (uint8, uint8) {
	a := n % 10
	if n < 10 {
		return a, 0
	}

	b := n / 10 % 10
	return b, a
}

func compare(list, buff []uint8) bool {
	ns := len(buff)
	for i := 0; i < ns; i++ {
		if list[i] != buff[i] {
			return false
		}
	}
	return true
}

func solve(line string) int {
	list := []uint8{3, 7}

	buff := []uint8{}
	for _, ch := range line {
		val, _ := strconv.Atoi(string(ch))
		buff = append(buff, uint8(val))
	}

	fmt.Println(buff)
	ns := len(buff)

	p1 := 0
	p2 := 1

	for {
		new := list[p1] + list[p2]
		n1, n2 := split(new)

		if new < 10 {
			list = append(list, n1)

			idx := len(list) - ns
			if len(list) >= ns && compare(list[idx:], buff) {
				return len(list[:len(list)-ns])
			}

		} else {
			list = append(list, n1, n2)

            // The 'target' buff can be in two posisions when we add two
            // number, I.e. either [_ _ _ _ _ n2] or [x _ _ _ _ _] where 
            // the number of '_'s = len(buff)
			idx := len(list) - ns
			if len(list) >= ns && compare(list[idx:], buff) {
				return len(list[:len(list)-ns])
			}
			if idx-1 >= 0 && len(list[idx-1:]) >= ns && compare(list[idx-1:], buff) {
				return len(list[:idx-1])
			}
		}

		p1 = (p1 + int(list[p1]) + 1) % len(list)
		p2 = (p2 + int(list[p2]) + 1) % len(list)
	}
}

func partOne(n int) string {
	list := []uint8{3, 7}

	p1 := 0
	p2 := 1

	for len(list) < n+10 {
		new := list[p1] + list[p2]
		n1, n2 := split(new)

		if new < 10 {
			list = append(list, n1)
		} else {
			list = append(list, n1, n2)
		}

		p1 = (p1 + int(list[p1]) + 1) % len(list)
		p2 = (p2 + int(list[p2]) + 1) % len(list)
	}

	// If its odd, we've generated one too much number
	s := ""
	for _, v := range list[n : n+10] {
		s += fmt.Sprintf("%d", v)
	}
	return s
}

func partTwo(line string) int {
	numSteps := solve(line)
	return numSteps
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
	line := array[0]
	num, _ := strconv.Atoi(line)
	timeFunc(Silver, partOne, num)
	timeFunc(Gold, partTwo, line)
}
