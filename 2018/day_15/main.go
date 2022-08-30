package main

import (
	"fmt"
	"image"
	"os"
	"sort"
	"strings"
	"time"
)

func readLine(path string) []string {
	b, _ := os.ReadFile(path)
	str := string(b)
	return strings.Split(str, "\n")
}

func isEmptyField(np image.Point, grid *[]string, fs map[image.Point]fighter) bool {
	_, ok := fs[np]
	if ok {
		return false
	}
	return np.X >= 0 && np.X < len((*grid)[0]) && np.Y >= 0 && np.Y < len(*grid) && (*grid)[np.Y][np.X] == '.'
}

func minHealth(fs []fighter) int {
	min := fs[0].health
	for _, f := range fs {
		if f.health < min {
			min = f.health
		}
	}
	return min
}

func adjacentEnemy(fs map[image.Point]fighter, pt image.Point, target byte) (bool, image.Point) {
	es := []fighter{}
	for _, inc := range []image.Point{{-1, 0}, {1, 0}, {0, -1}, {0, 1}} {
		np := pt.Add(inc)
		if f, ok := fs[np]; ok && f.sym == target {
			es = append(es, f)
		}
	}
	if len(es) > 0 {
		health := []fighter{}

		min := minHealth(es)
		for _, f := range es {
			if f.health == min {
				health = append(health, f)
			}
		}
		sort.Slice(health, func(i, j int) bool { return s(health[i].pos, health[j].pos) })
		return true, health[0].pos
	}
	return false, pt
}

type state struct {
	firstMove image.Point
	numMoves  int
	tileCoord image.Point
}

func contains(p image.Point, ps []image.Point) bool {
	for _, v := range ps {
		if p == v {
			return true
		}
	}
	return false
}

func removeDup(stack []image.Point) []image.Point {
	set := []image.Point{}
	for _, v := range stack {
		if !contains(v, set) {
			set = append(set, v)
		}
	}
	return set
}

func checkWin(fs map[image.Point]fighter, target byte) bool {
	for _, f := range fs {
		if f.sym == target {
			return false
		}
	}
	return true
}

func bfs(grid *[]string, fs map[image.Point]fighter, start image.Point, target byte) (Choice, image.Point) {
	if checkWin(fs, target) {
		return Complete, start
	}

	if succ, pt := adjacentEnemy(fs, start, target); succ {
		return Attack, pt
	}

	firstMoves := []image.Point{}
	for _, inc := range []image.Point{{1, 0}, {-1, 0}, {0, -1}, {0, 1}} {
		np := start.Add(inc)
		if isEmptyField(np, grid, fs) {
			firstMoves = append(firstMoves, np)
		}
	}

	bestMove := []state{}

	for _, move := range firstMoves {
		if ok, _ := adjacentEnemy(fs, move, target); ok {
			bestMove = append(bestMove, state{move, 1, move})
		}
		seenCoordinates := []image.Point{start, move}
		stack := []image.Point{}
		for _, inc := range []image.Point{{1, 0}, {-1, 0}, {0, -1}, {0, 1}} {
			np := move.Add(inc)
			if isEmptyField(np, grid, fs) && !contains(np, seenCoordinates) {
				stack = append(stack, np)
			}
		}

		i := 1
		run := true

		for run {
			i += 1
			newStack := []image.Point{}
			for _, tile := range stack {
				if contains(tile, seenCoordinates) {
					continue
				}
				seenCoordinates = append(seenCoordinates, tile)

				if ok, _ := adjacentEnemy(fs, tile, target); ok {
					bestMove = append(bestMove, state{move, i, tile})
					run = false
					continue
				}
				for _, inc := range []image.Point{{-1, 0}, {1, 0}, {0, -1}, {0, 1}} {
					np := tile.Add(inc)
					if isEmptyField(np, grid, fs) && !contains(np, seenCoordinates) {
						newStack = append(newStack, np)
					}
				}
			}
			stack = removeDup(newStack)

			if len(stack) == 0 {
				run = false
			}

		}
	}
	return getBestMove(bestMove)
}

func minStep(bestMoves []state) int {
	min := bestMoves[0].numMoves
	for _, v := range bestMoves {
		if v.numMoves < min {
			min = v.numMoves
		}
	}
	return min
}

func getBestMove(bestMoves []state) (Choice, image.Point) {
	if len(bestMoves) == 0 {
		return Wait, image.Pt(0, 0)
	}

	// First condition: fewest numbers of moves
	min := minStep(bestMoves)
	minMoves := []state{}
	for _, v := range bestMoves {
		if v.numMoves == min {
			minMoves = append(minMoves, v)
		}
	}

	sort.Slice(minMoves, func(i, j int) bool { return s(minMoves[i].tileCoord, minMoves[j].tileCoord) })
	tile := []state{}
	for _, v := range minMoves {
		if v.tileCoord == minMoves[0].tileCoord {
			tile = append(tile, v)
		}
	}

	sort.Slice(tile, func(i, j int) bool { return s(tile[i].firstMove, tile[j].firstMove) })
	tile2 := []state{}
	for _, v := range tile {
		if v.firstMove == tile[0].firstMove {
			tile2 = append(tile2, v)
		}
	}

	return Move, tile2[0].firstMove
}

type State struct {
	cost int
	pos  image.Point
}

func s(pos1 image.Point, pos2 image.Point) bool {
	if pos1.Y < pos2.Y {
		return true
	} else if pos1.Y > pos2.Y {
		return false
	} else {
		return pos1.X < pos2.X
	}
}

type Choice = int

const (
	Move Choice = iota
	Attack
	Wait
	Complete
)

type fighter struct {
	sym    byte
	pos    image.Point
	attack int
	health int
	id     int
}

func sumHealth(fs map[image.Point]fighter) int {
	sum := 0
	for _, f := range fs {
		if f.health > 0 {
			sum += f.health
		}
	}
	return sum
}

func solve(arr []string, eAttack int, earlyExit func(fighter) bool) (bool, int) {
	id := 0
	fighters := map[image.Point]fighter{}
	numElves := 0

	array := []string{}
	builder := strings.Builder{}
	for y := range arr {
		builder.Reset()
		for x := range arr[y] {
			sv := arr[y][x]
			attack := 3
			if sv == 'G' || sv == 'E' {
				if sv == 'E' {
					numElves += 1
					attack = eAttack
				}
				pt := image.Pt(x, y)
				fighters[pt] = fighter{sym: sv, pos: pt, attack: attack, health: 200, id: id}
				id += 1
				builder.WriteByte('.')
			} else {
				builder.WriteByte(sv)
			}
		}
		array = append(array, builder.String())
	}

	for i := 0; ; i++ {
		fs := []fighter{}
		for _, f := range fighters {
			fs = append(fs, f)
		}
		sort.Slice(fs, func(i, j int) bool { return s(fs[i].pos, fs[j].pos) })

		for _, f := range fs {
			other := byte('G')
			if f.sym == 'G' {
				other = 'E'
			}

			// Fighter is ded
			if ftemp, ok := fighters[f.pos]; !ok || ftemp.id != f.id {
				continue
			} else {
				// Whoops..! the figher could have lost health
				// Need to update.!
				f.health = ftemp.health
			}

			opt, pos := bfs(&array, fighters, f.pos, other)

			switch opt {
			case Attack:
				enemy := fighters[pos]
				enemy.health -= f.attack
				if enemy.health <= 0 {

					// Part two optimization
					if earlyExit(enemy) {
						return false, 0
					}
					delete(fighters, pos)
				} else {
					fighters[pos] = enemy
				}

			case Move:
				delete(fighters, f.pos)
				f.pos = pos
				fighters[pos] = f

				if ok, pt := adjacentEnemy(fighters, pos, other); ok {
					enemy := fighters[pt]
					enemy.health -= f.attack
					if enemy.health <= 0 {
						if earlyExit(enemy) {
							return false, 0
						}
						delete(fighters, pt)
					} else {
						fighters[pt] = enemy
					}
				}
			case Wait:
				// Do nothing, really
			case Complete:
				sh := sumHealth(fighters)
				return f.sym == 'E' && numElves == len(fighters), i * sh
			}
		}
	}
}

func partOne(arr []string) int {
	_, res := solve(arr, 3, func(f fighter) bool { return false })
	return res
}

func partTwo(array []string) int {
	for i := 4; ; i++ {
		if ok, res := solve(array, i, func(f fighter) bool { return f.sym == 'E' }); ok {
			return res
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
