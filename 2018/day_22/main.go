package main

import (
	"container/heap"
	"fmt"
	"image"
	"os"
	"strings"
	"time"
)

func readLine(path string) []string {
	b, _ := os.ReadFile(path)
	str := string(b)
	return strings.Split(str, "\n")
}

func geoIndex(x, y, tx, ty, d int, seen map[image.Point]int) int {
	if x == 0 && y == 0 {
		return 0
	} else if x == tx && y == ty {
		return 0
	} else if y == 0 {
		return x * 16807
	} else if x == 0 {
		return y * 48271
	} else {
		// If we have not seen it before
		if val, ok := seen[image.Pt(x, y)]; !ok {
			xx := erosionLevel(x-1, y, tx, ty, d, seen)
			yy := erosionLevel(x, y-1, tx, ty, d, seen)

			newVal := xx * yy
			seen[image.Pt(x, y)] = newVal
			return newVal
		} else {
			return val
		}
	}
}

type ErosionLevel = int

const (
	Rocky  ErosionLevel = 0
	Wet                 = 1
	Narrow              = 2
)

func erosionLevel(x, y, tx, ty, depth int, seen map[image.Point]int) ErosionLevel {
	return (geoIndex(x, y, tx, ty, depth, seen) + depth) % 20183
}

func parse(array []string) (int, int, int) {
	var depth, targetx, targety int
	fmt.Sscanf(array[0], "depth: %d", &depth)
	fmt.Sscanf(array[1], "target: %d,%d", &targetx, &targety)
	return depth, targetx, targety
}

func partOne(array []string) int {
	d, tx, ty := parse(array)
	seen := map[image.Point]int{}

	risk := 0
	for y := 0; y <= ty; y++ {
		for x := 0; x <= tx; x++ {
			val := erosionLevel(x, y, tx, ty, d, seen) % 3
			risk += val
		}
	}
	return risk
}

type Gear = int

const (
	Torch Gear = iota
	Climbing
	Neither
)

type state struct {
	pos  image.Point
	cost int
	gear Gear
}

func (s state) HigherPriorityThan(other Interface) bool {
	return s.cost < other.(state).cost
}

func otherGears(nextRoom ErosionLevel) []Gear {
	if nextRoom == Rocky {
		return []Gear{Climbing, Torch}
	} else if nextRoom == Wet {
		return []Gear{Climbing, Neither}
	} else {
		return []Gear{Torch, Neither}
	}
}

type State2 struct {
	gear Gear
	pos  image.Point
}

func toState2(state state) State2 {
	return State2{gear: state.gear, pos: state.pos}
}

func validTool(gear Gear, next ErosionLevel, pos image.Point, target image.Point) bool {
	if pos == image.Pt(0, 0) || pos == target {
		return gear == Torch
	} else {
		if next == Rocky {
			return gear == Climbing || gear == Torch
		} else if next == Wet {
			return gear == Climbing || gear == Neither
		} else {
			return gear == Torch || gear == Neither
		}
	}
}

func bfs(tx, ty, d int, cache map[image.Point]int) int {
	pq := New()
	zero := image.Pt(0, 0)
	start := state{pos: zero, cost: 0, gear: Torch}
	pq.Push(start)
	seen := map[image.Point]int{}

	seenMinimumCost := map[State2]int{toState2(start): 0}

	end := image.Pt(tx, ty)

	for pq.queue.Len() > 0 {
		val := pq.Pop().(state)

		if val.pos == end && val.gear == Torch {
			return val.cost
		}

		candidates := []state{}

		for _, v := range []image.Point{{-1, 0}, {1, 0}, {0, -1}, {0, 1}} {
			new := val.pos.Add(v)
			if new.X >= 0 && new.Y >= 0 {
				nextRoom := erosionLevel(new.X, new.Y, tx, ty, d, seen) % 3

				if validTool(val.gear, nextRoom, new, end) {
					ns := val
					ns.cost += 1
					ns.pos = new
					candidates = append(candidates, ns)
				}
			}
		}

		currRoom := erosionLevel(val.pos.X, val.pos.Y, tx, ty, d, seen) % 3
		for _, g := range otherGears(currRoom) {
			ns := val
			ns.cost += 7
			ns.gear = g
			candidates = append(candidates, ns)
		}

		for _, nextState := range candidates {
			state2 := toState2(nextState)
			vv, ok := seenMinimumCost[state2]
			if !ok || vv > nextState.cost {
				pq.Push(nextState)
				seenMinimumCost[state2] = nextState.cost
			}
		}
	}

	// unreachable
	return -1
}

func partTwo(array []string) int {
	d, tx, ty := parse(array)
	return bfs(tx, ty, d, map[image.Point]int{})
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

type Interface interface {
	HigherPriorityThan(Interface) bool
}

const shrinkMinCap = 1000
const shrinkNewSizeFactor = 2
const shrinkCapLenFactorCondition = 4

type Queue struct {
	queue heap.Interface
}

func New() *Queue {
	pq := &Queue{}
	pq.queue = newHeapMemory(
		shrinkMinCap,
		shrinkNewSizeFactor,
		shrinkCapLenFactorCondition,
	)
	return pq
}

func (pq *Queue) Push(something Interface) {
	heap.Push(pq.queue, something)
}

func (pq *Queue) Pop() Interface {

	if pq.queue.Len() <= 0 {
		return nil
	}
	r := heap.Pop(pq.queue)
	return r.(Interface)
}

type heapMemory struct {
	slice                       internalSlice
	ShrinkMinCap                int
	ShrinkNewSizeFactor         int
	ShrinkCapLenFactorCondition int
}

func newHeapMemory(shrinkMinCap, shrinkNewSizeFactor, shrinkCapLenFactorCondition int) *heapMemory {
	return &heapMemory{
		slice:                       make(internalSlice, 0),
		ShrinkMinCap:                shrinkMinCap,
		ShrinkNewSizeFactor:         shrinkNewSizeFactor,
		ShrinkCapLenFactorCondition: shrinkCapLenFactorCondition,
	}
}

type internalSlice []Interface

func (pq *heapMemory) Len() int { return len(pq.slice) }

func (pq *heapMemory) Less(i, j int) bool {
	return pq.slice[i].HigherPriorityThan(pq.slice[j])
}

func (pq *heapMemory) Swap(i, j int) {
	pq.slice[i], pq.slice[j] = pq.slice[j], pq.slice[i]
}

func (pq *heapMemory) Push(x interface{}) {
	pq.slice = append(pq.slice, x.(Interface))
}

func (pq *heapMemory) Pop() interface{} {
	old, n := pq.shrinkIfNeeded()
	item := (*old)[n-1]
	pq.slice = (*old)[0 : n-1]
	return item
}

func (pq *heapMemory) shrinkIfNeeded() (*internalSlice, int) {
	l, c := len(pq.slice), cap(pq.slice)
	if cap(pq.slice) > pq.ShrinkMinCap && c/l > pq.ShrinkCapLenFactorCondition {
		newSlice := make(internalSlice, pq.ShrinkNewSizeFactor*l, pq.ShrinkNewSizeFactor*l)
		for i, v := range pq.slice {
			newSlice[i] = v
		}
		return &newSlice, l
	}
	return &pq.slice, l
}
