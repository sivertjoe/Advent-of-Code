package main

import (
	"bufio"
	"fmt"
	"os"
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

func searchForSingleNumber(array []int, num int) bool {
	for _, count := range array {
		if count == num {
			return true
		}
	}
	return false
}

func partOne(array []string) int {
	two := 0
	three := 0

	for _, v := range array {
		var local [26]int
		for _, char := range v {
			idx := char - 'a'
			local[idx] += 1
		}
		if searchForSingleNumber(local[:], 2) {
			two += 1
		}
		if searchForSingleNumber(local[:], 3) {
			three += 1
		}

	}

	return two * three
}

func differsByOne(first string, second string) (string, bool) {
	state := 0

	fmt.Println(first)
	fmt.Println(second)
	fmt.Println()

	res := ""
	for i := 0; i < len(first); i += 1 {
		if first[i] != second[i] {
			state += 1
		} else {
			res += string(first[i])
		}

		if state == 2 {
			return "", false
		}
	}

	return res, true
}

func partTwo(array []string) string {
	for i := 0; i < len(array); i += 1 {
		first := array[i]
		for j := i + 1; j < len(array); j += 1 {
			second := array[j]
			res, found := differsByOne(first, second)
			if found {
				return res
			}
		}
	}

	return ""
}

func main() {
	array := readLine("input")
	fmt.Println(partOne(array))
	fmt.Println(partTwo(array))
}
