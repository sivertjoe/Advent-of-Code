package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"time"
)

func readFile() []string {
	f, err := os.Open("input")

	if err != nil {
		log.Fatal(err)
	}

	defer f.Close()

	data := []string{}

	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		data = append(data, scanner.Text())
	}

	if err := scanner.Err(); err != nil {
		log.Fatal(err)
	}

	return data
}
func timeFunc[T any, S any](f func(T) S, arg T) {
	t0 := time.Now()
	res := f(arg)
	elapsed := time.Now().Sub(t0).Milliseconds()
	fmt.Printf("(%dms)\t%v\n", elapsed, res)
}

func main() {
	data := readFile()
    timeFunc(partOne, data)
    timeFunc(partTwo, data)
}

func traverse(data []string, right int, down int) int {
	posX := 0
	posY := 0
	counter := 0

	for posY < len(data) {
		if data[posY][posX] == '#' {
			counter += 1
		}

		posX = (posX + right) % len(data[0])
		posY += down
	}

	return counter
}

func partOne(data []string) int {
	return traverse(data, 3, 1)
}

func partTwo(data []string) int {
	vals := []int{1, 1, 3, 1, 5, 1, 7, 1, 1, 2}
	sum := 1
	for i := 0; i < len(vals); i += 2 {
		x := vals[i]
		y := vals[i+1]
		sum *= traverse(data, x, y)
	}
	return sum
}
