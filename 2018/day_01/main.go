package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"time"
)

func readLine(path string) []string {

	array := []string{}
	file, err := os.Open(path)
	if err != nil {
		fmt.Println(err.Error() + `: ` + path)
		os.Exit(-1)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		array = append(array, scanner.Text())
	}
	return array
}

func partOne(array []string) int {
	sum := 0
	for _, v := range array {
		num, _ := strconv.Atoi(v)
		sum += num
	}
	return sum
}

func partTwo(array []string) int {
	seen := make(map[int]int)
	sum := 0
	seen[0] = 0

	for {
		for _, v := range array {
			num, _ := strconv.Atoi(v)
			sum += num

			i, ok := seen[sum]
			if ok {
				return i
			}

			seen[sum] = sum

		}
	}
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
