package main

import (
	"fmt"
	"os"
	"strings"
)

func readLine(path string) []string {
	b, _ := os.ReadFile(path)
	str := string(b)
	split := strings.Split(str, "\n")

	return split[:len(split)-1]
}

type pos struct {
	x  int
	y  int
	vx int
	vy int
}

func dims(ps []pos) int {
	minX := 0
	minY := 0

	maxX := 0
	maxY := 0

	for _, p := range ps {
		if p.x < minX {
			minX = p.x
		}
		if p.x > maxX {
			maxX = p.x
		}
		if p.y < minY {
			minY = p.y
		}
		if p.y > maxY {
			maxY = p.y
		}
	}

	return (maxX - minX) * (maxY - minY)
}

func print(ps []pos) string {
	minX := ps[0].x
	minY := ps[0].y

	maxX := ps[0].x
	maxY := ps[0].y

	for _, p := range ps {
		if p.x < minX {
			minX = p.x
		}
		if p.x > maxX {
			maxX = p.x
		}
		if p.y < minY {
			minY = p.y
		}
		if p.y > maxY {
			maxY = p.y
		}
	}

	w := maxX - minX
	h := maxY - minY

	buffer := make([][]byte, h+1)
	for i := range buffer {
		buffer[i] = make([]byte, w+1)
		for x := 0; x < w+1; x++ {
			buffer[i][x] = '.'
		}
	}

	for _, p := range ps {
		buffer[p.y-minY][p.x-minX] = '#'
	}

	var builder strings.Builder
	for y := 0; y < h+1; y++ {
		for x := 0; x < w+1; x++ {
			builder.WriteByte(buffer[y][x])
		}
		builder.WriteByte('\n')
	}
	return builder.String()
}

func update(ps []pos) []pos {
	for i := 0; i < len(ps); i++ {
		ps[i].x += ps[i].vx
		ps[i].y += ps[i].vy
	}
	return ps
}

func solve(array []string) (string, int) {
	p := []pos{}
	for _, line := range array {
		var pos pos
		fmt.Sscanf(line, "position=<%d, %d> velocity=<%d, %d>", &pos.x, &pos.y, &pos.vx, &pos.vy)
		p = append(p, pos)
	}

	secs := 0
	for {
		before := dims(p)
		p = update(p)
		after := dims(p)
		if after > before {
			break
		}
		secs++
	}
	for i := 0; i < len(p); i++ {
		p[i].x -= p[i].vx
		p[i].y -= p[i].vy
	}
	return print(p), secs
}

func partOne(array []string) string {
	msg, _ := solve(array)
	return msg
}

func partTwo(array []string) int {
	_, secs := solve(array)
	return secs
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
