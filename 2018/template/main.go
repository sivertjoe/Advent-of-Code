package main

import (
	"fmt"
	"os"
	"strings"
)

func readLine(path string) []string {
	b, _ := os.ReadFile(path)
	str := string(b)
	return strings.Split(str, "\n")
}

func partOne(array []string) int {
	return 0
}

func partTwo(array []string) int {
	return 0
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
