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

type Instruction = uint8

var mapper map[string]Instruction = map[string]Instruction{
	"addi": Addi,
	"addr": Addr,
	"mulr": Mulr,
	"muli": Muli,
	"banr": Banr,
	"bani": Bani,
	"borr": Borr,
	"bori": Bori,
	"setr": Setr,
	"seti": Seti,
	"gtir": Gtir,
	"gtri": Gtri,
	"gtrr": Gtrr,
	"eqir": Eqir,
	"eqri": Eqri,
	"eqrr": Eqrr}

const (
	Addr Instruction = 1
	Addi             = 2
	Mulr             = 3
	Muli             = 4
	Banr             = 5
	Bani             = 6
	Borr             = 7
	Bori             = 8
	Setr             = 9
	Seti             = 10
	Gtir             = 11
	Gtri             = 12
	Gtrr             = 13
	Eqir             = 14
	Eqri             = 15
	Eqrr             = 16
)

func conv(b bool) int {
	var n int
	if b {
		n = 1
	}
	return n
}

func execute(is *[]instruction, reg [6]int, ipf int) [6]int {
	ip := reg[ipf]
	opcode := (*is)[ip].ins
	ins := (*is)[ip].rest

	switch opcode {
	case Eqir:
		reg[ins[3]] = conv(ins[1] == reg[ins[2]])
	case Eqri:
		reg[ins[3]] = conv(reg[ins[1]] == ins[2])
	case Eqrr:
		reg[ins[3]] = conv(reg[ins[1]] == reg[ins[2]])
	case Gtir:
		reg[ins[3]] = conv(ins[1] > reg[ins[2]])
	case Gtri:
		reg[ins[3]] = conv(reg[ins[1]] > ins[2])
	case Gtrr:
		reg[ins[3]] = conv(reg[ins[1]] > reg[ins[2]])
	case Setr:
		reg[ins[3]] = reg[ins[1]]
	case Seti:
		reg[ins[3]] = ins[1]
	case Borr:
		reg[ins[3]] = reg[ins[1]] | reg[ins[2]]
	case Bori:
		reg[ins[3]] = reg[ins[1]] | ins[2]
	case Banr:
		reg[ins[3]] = reg[ins[1]] & reg[ins[2]]
	case Bani:
		reg[ins[3]] = reg[ins[1]] & ins[2]
	case Addr:
		reg[ins[3]] = reg[ins[1]] + reg[ins[2]]
	case Addi:
		reg[ins[3]] = reg[ins[1]] + ins[2]
	case Mulr:
		reg[ins[3]] = reg[ins[1]] * reg[ins[2]]
	case Muli:
		reg[ins[3]] = reg[ins[1]] * ins[2]
	}
	return reg
}

type instruction struct {
	ins Instruction

	// We actually just need 3, but I copied the code above from day 16, and I am lazy
	rest [4]int
}

func getIns(array []string) (int, []instruction) {
	in := []instruction{}

	var ip int
	fmt.Sscanf(array[0], "#ip %d", &ip)

	for i := 1; i < len(array); i++ {
		newIns := instruction{}
		var s string
		fmt.Sscanf(array[i], "%s %d %d %d", &s, &newIns.rest[1], &newIns.rest[2], &newIns.rest[3])
		newIns.ins = mapper[s]
		in = append(in, newIns)
	}
	return ip, in
}

func partOne(array []string) int {
	reg := [6]int{}
	ipf, ins := getIns(array)

	for reg[ipf] < len(ins) {
		reg = execute(&ins, reg, ipf)
		reg[ipf]++
	}
	return reg[0]
}

/*
Okay, after having analyzed the instructions _by hand_, I found that it enters
a loop. Before entering the loop we have the registers:
[0 1 4 1 1 z], where z is a big number; 10551355 in our case. We can find this
number by running the instructions until ip = 34.

Then, the registers follows this loop that starts at ip = 2. The pattern of the
loop is:
[S, X, 3, i, i * X, z], where i is ever increasing.
X is incremented when i * X > z.
S is just some number; starting at 1.

Importantly, when i * X mod z == 0, i is added to S.

This giant loop ends when X == z.

# The final value of S, our puzzle answer, is therefore

S = every number from 1 up to and including z, when `n` mod z == 0
*/
func partTwo(array []string) int {
	reg := [6]int{}
	ipf, ins := getIns(array)

	// Part 2:
	reg[0] = 1

	seen := map[int]bool{}

	for !seen[reg[ipf]] {
		seen[reg[ipf]] = true
		reg = execute(&ins, reg, ipf)
		reg[ipf]++

	}

	// z is the big number, depending on the input, it can be
	// in different registers.
	z := reg[0]
	for _, r := range reg {
		if r > z {
			z = r
		}
	}

	sum := 0
	for i := 1; i <= z; i++ {
		if z%i == 0 {
			sum += i
		}
	}

	return sum
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
