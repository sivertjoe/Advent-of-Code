package main

import (
	"fmt"
	"os"
	"strings"
	"time"
)

func readLine(path string) []string {
	b, _ := os.ReadFile(path)
	str := string(b)
	s := strings.Split(str, "\n")
	return s[:len(s)-1]
}

func countAdjacent(array *[][]byte, x int, y int, sym byte) int {
	sum := 0
	for dy := -1; dy <= 1; dy++ {
		for dx := -1; dx <= 1; dx++ {
			nx := x + dx
			ny := y + dy
			if dx == 0 && dy == 0 {
				continue
			}
			if nx >= 0 && ny >= 0 && nx < len((*array)[0]) && ny < len(*array) {
				if (*array)[ny][nx] == sym {
					sum += 1
				}
			}

		}
	}
	return sum
}

func print(array *[][]byte) {
	for y := range *array {
		for x := range (*array)[y] {
			sv := (*array)[y][x]
			fmt.Printf("%c", sv)
		}
		fmt.Println()
	}
	fmt.Println()
}

func firstNonDot(buff *[][]byte) int {
	for x := 0; x < len(*buff); x++ {
		if (*buff)[0][x] != '.' {
			return x
		}
	}
	return -1
}

func score(array *[][]byte) int {
	lumber := 0
	three := 0
	for y := range *array {
		for x := range (*array)[y] {
			sv := (*array)[y][x]
			if sv == '|' {
				three += 1
			} else if sv == '#' {
				lumber += 1
			}
		}
	}
	return three * lumber
}

func solve(array []string, gen int) int {
	buff1 := make([][]byte, len(array))
	buff2 := make([][]byte, len(array))
	for i, v := range array {
		buff1[i] = []byte(v)
		buff2[i] = []byte(v)
	}

	scores := []int{}

	const N = 1000

	for i := 0; i < gen; i++ {
		// Make the damn array
		for y := range buff1 {
			copy(buff2[y], buff1[y])
		}

		for y := range buff1 {
			for x := range (buff1)[y] {
				sv := (buff1)[y][x]
				threeCount := countAdjacent(&buff1, x, y, '|')
				lumberyardCount := countAdjacent(&buff1, x, y, '#')

				if sv == '.' && threeCount >= 3 {
					buff2[y][x] = '|'
				} else if sv == '|' && lumberyardCount >= 3 {
					buff2[y][x] = '#'
				} else if sv == '#' && (lumberyardCount < 1 || threeCount < 1) {
					buff2[y][x] = '.'
				}
			}
		}

		sc := score(&buff2)
		scores = append(scores, sc)
		if i > N+1 {
			if sc == scores[N] {
				period := i - N
				valid := true

				for j := 1; j < 10; j++ {
					if scores[i-j] != scores[i-j-period] {
						valid = false
						break
					}
				}

				if valid {
					return scores[N+(gen-N)%period-1]
				}
			}
		}

		for y := range buff1 {
			copy(buff1[y], buff2[y])
		}
	}
	return score(&buff1)
}

func partOne(array []string) int {
	return solve(array, 10)
}

func partTwo(array []string) int {
	return solve(array, 1000000000)
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
