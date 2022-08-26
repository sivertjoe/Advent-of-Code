package main

import (
	"fmt"
	"os"
	"strings"
	"time"
)

func readLine(path string) string {
	b, _ := os.ReadFile(path)
	return string(b[:len(b)-1])
}

func removeUnits(line string, p1 rune, p2 rune) string {
	var sb strings.Builder
	for _, c := range line {
		if c != p1 && c != p2 {
			sb.WriteRune(c)
		}
	}
	return sb.String()
}

func calc(line string) int {
	abs := func(b int32) int32 {
		if b < 0 {
			return -b
		}
		return b
	}

	for {
		var builder strings.Builder
		for i := 0; i < len(line)-1; {
			c1 := int32(line[i])
			c2 := int32(line[i+1])
			diff := abs(c1 - c2)

			if diff == 32 {
				i += 2
			} else {
				builder.WriteByte(line[i])
				i += 1
				if i == len(line)-1 {
					builder.WriteByte(line[i])
				}
			}
		}
		temp := builder.String()
		if temp == line {
			break
		}
		line = temp
	}

	return len(line)
}

func partOne(line string) int {
	return calc(line)
}

func partTwo(line string) int {
	min := calc(removeUnits(line, 'a', 'A'))
	diff := 'a' - 'A'

	for ch := 'b'; ch <= 'z'; ch++ {
		newCalc := calc(removeUnits(line, ch, ch-diff))
		if newCalc < min {
			min = newCalc
		}
	}
	return min
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

	line := readLine(args[1])
	timeFunc(Silver, partOne, line)
	timeFunc(Gold, partTwo, line)
}
