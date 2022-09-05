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
	s    string
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
		newIns.s = s
		in = append(in, newIns)
	}
	return ip, in
}

func c(b bool) int {
	if b {
		return 1
	}
	return 0
}

// This code works, assuming you give it the correct register and instruction
// for when register 0 is touched. However, it is slow af. Therefore solve
// is 'hardcoded' version of the program that does the same.

// func solve2(array []string, exitEarly bool, register int, instruction int) int {
// 	reg := [6]int{}
// 	ipf, ins := getIns(array)
//
// 	seen := map[int]bool{}
// 	prev := 0
// 	for reg[ipf] < len(ins) {
// 		if reg[ipf] == instruction {
// 			if exitEarly {
// 				return reg[register]
// 			} else if seen[reg[register]] {
// 				return prev
// 			} else {
// 				seen[reg[register]] = true
// 				prev = reg[register]
// 			}
// 		}
// 		reg = execute(&ins, reg, ipf)
// 		reg[ipf]++
// 	}
// 	return reg[0]
// }

// This is highly individual input.
// Not sure this will work for every code.
// _Maybe_ the structure is similar?
// The 'magic numbers' most definantly are not.
func solve(exitEarly bool) int {
	var r1, r2, r3, r4 int

	seen := map[int]bool{}
	prev := 0
	for {
		r3 = r4 | 65536
		r4 = 14464005

		for {
			r4 = ((((r3 & 255) + r4) & 16777215) * 65899) & 16777215
			if r3 < 256 {
				break
			}
			r2 = 0

			for {
				r1 = (r2 + 1) * 256
				if r1 > r3 {
					break
				}
				r2 = r2 + 1
			}
			r3 = r2
		}
		if exitEarly {
			return r4
		}
		if seen[r4] {
			return prev
		} else {
			seen[r4] = true
			prev = r4
		}
		r2 = 0
	}
}

func partOne(array []string) int {
	return solve(true)
}

func partTwo(array []string) int {
	return solve(false)
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
