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

func partOne(array []string) int {
	return 0
}

func partTwo(array []string) int {
	return 0
}

func main() {
	array := readLine("input")
	fmt.Println(partOne(array))
	fmt.Println(partTwo(array))
}
