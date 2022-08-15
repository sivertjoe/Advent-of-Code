package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
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

func main() {
	array := readLine("input")
	fmt.Println(partOne(array))
	fmt.Println(partTwo(array))
}
