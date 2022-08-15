package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
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

type Claim struct {
	id int
	x  int
	y  int
	w  int
	h  int
}

func parseToClaim(s string) Claim {
	claim := Claim{}

	first := strings.Index(s, " ")
	val, _ := strconv.Atoi(s[1:first])
	claim.id = val

	at := strings.Index(s, "@ ")
	comma := strings.Index(s, ",")

	n1, _ := strconv.Atoi(s[at+2 : comma])
	claim.x = n1

	colon := strings.Index(s, ":")
	n2, _ := strconv.Atoi(s[comma+1 : colon])
	claim.y = n2

	x := strings.Index(s, "x")
	n3, _ := strconv.Atoi(s[colon+2 : x])
	claim.w = n3

	n4, _ := strconv.Atoi(s[x+1:])
	claim.h = n4

	return claim
}

func partOne(array []string) int {
	var suit [1000][1000]int

	for _, v := range array {
		claim := parseToClaim(v)

		for y := claim.y; y < claim.y+claim.h; y += 1 {
			for x := claim.x; x < claim.x+claim.w; x += 1 {
				suit[y][x] += 1
			}
		}
	}

	sum := 0
	for _, c := range suit {
		for _, count := range c {
			if count > 1 {
				sum += 1
			}
		}
	}
	return sum
}

type Point struct {
	id    int
	count int
}

func noOverlap(array [1000][1000]Point, claim Claim) bool {
	for y := claim.y; y < claim.y+claim.h; y += 1 {
		for x := claim.x; x < claim.x+claim.w; x += 1 {
			p := array[y][x]
			if p.count > 1 {
				return false
			}
		}
	}
	return true
}

func partTwo(array []string) int {
	var suit [1000][1000]Point

	for _, v := range array {
		claim := parseToClaim(v)

		for y := claim.y; y < claim.y+claim.h; y += 1 {
			for x := claim.x; x < claim.x+claim.w; x += 1 {
				p := suit[y][x]
				p.id = claim.id
				p.count += 1
				suit[y][x] = p
			}
		}
	}

	for _, v := range array {
		claim := parseToClaim(v)
		if noOverlap(suit, claim) {
			return claim.id
		}
	}
	return -1

}

func main() {
	array := readLine("input")
	fmt.Println(partOne(array))
	fmt.Println(partTwo(array))
}
