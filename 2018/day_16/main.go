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

type testInstruction struct {
	before [4]int
	ins    [4]int
	after  [4]int
}

/*
Before: [3, 2, 1, 1]
9 2 1 2
After:  [3, 2, 2, 1]

addr (add register) stores into register C the result of adding register A and register B.
addi (add immediate) stores into register C the result of adding register A and value B.


mulr (multiply register) stores into register C the result of multiplying register A and register B.
muli (multiply immediate) stores into register C the result of multiplying register A and value B.

banr (bitwise AND register) stores into register C the result of the bitwise AND of register A and register B.
bani (bitwise AND immediate) stores into register C the result of the bitwise AND of register A and value B.

borr (bitwise OR register) stores into register C the result of the bitwise OR of register A and register B.
bori (bitwise OR immediate) stores into register C the result of the bitwise OR of register A and value B.

setr (set register) copies the contents of register A into register C. (Input B is ignored.)
seti (set immediate) stores value A into register C. (Input B is ignored.)

gtir (greater-than immediate/register) sets register C to 1 if value A is greater than register B. Otherwise, register C is set to 0.
gtri (greater-than register/immediate) sets register C to 1 if register A is greater than value B. Otherwise, register C is set to 0.
gtrr (greater-than register/register) sets register C to 1 if register A is greater than register B. Otherwise, register C is set to 0.

eqir (equal immediate/register) sets register C to 1 if value A is equal to register B. Otherwise, register C is set to 0.
eqri (equal register/immediate) sets register C to 1 if register A is equal to value B. Otherwise, register C is set to 0.
eqrr (equal register/register) sets register C to 1 if register A is equal to register B. Otherwise, register C is set to 0.

*/

type Instruction = uint8

const (
	Addr Instruction = iota
	Addi
	Mulr
	Muli
	Banr
	Bani
	Borr
	Bori
	Setr
	Seti
	Gtir
	Gtri
	Gtrr
	Eqir
	Eqri
	Eqrr
)

func conv(b bool) int {
	var n int
	if b {
		n = 1
	}
	return n
}

func execute(opCode Instruction, ins [4]int, reg [4]int) [4]int {
	switch opCode {
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

func tryInstruction(ti testInstruction, ins Instruction) bool {
	ti.before = execute(ins, ti.ins, ti.before)
	return ti.before == ti.after
}

func getIns(array []string) ([]testInstruction, int) {
	ts := []testInstruction{}
	stopIndex := 0

	for i := 0; i >= 0 && i <= len(array)-3; i++ {
		if len(array[i]) == 0 && len(array[i+1]) == 0 && len(array[i+2]) == 0 {
			stopIndex = i + 3
			break
		} else if len(array[i]) == 0 {
			continue
		}
		var before [4]int
		fmt.Sscanf(array[i], "Before: [%d, %d, %d, %d]", &before[0], &before[1], &before[2], &before[3])

		var ins [4]int
		fmt.Sscanf(array[i+1], "%d %d %d %d", &ins[0], &ins[1], &ins[2], &ins[3])

		var after [4]int
		fmt.Sscanf(array[i+2], "After: [%d, %d, %d, %d]", &after[0], &after[1], &after[2], &after[3])

		ts = append(ts, testInstruction{before: before, ins: ins, after: after})
		i += 2
	}
	return ts, stopIndex
}

func partOne(array []string) int {
	ts, _ := getIns(array)
	sum := 0

	for _, t := range ts {
		count := 0
		for i := 0; i < 16; i++ {
			if tryInstruction(t, uint8(i)) {
				count += 1
			}
			if count == 3 {
				sum += 1
				break
			}
		}
	}
	return sum
}

func partTwo(array []string) int {
	ts, stop := getIns(array)

	ins := [16][]int{}

	for i, t := range ts {
		ins[t.ins[0]] = append(ins[t.ins[0]], i)
	}
	opCodeMap := [16][]Instruction{}

	// Assume the opcode op is the first instruction (Instruction(0))
	// Check if all testInstructions with opcode op passes the assumed instruction
	// If if fails, try the next instruction until success

	// op : [x, y, z], where (x|y|z|).ins[0] == op
	for op, in := range ins {
		opCode := -1

		// We want to figure out what instruction op matches to
		for v := 0; v < 16; v++ {
			opCode = v
			for _, i := range in {
				if !tryInstruction(ts[i], Instruction(v)) {
					// op code = v is incorrect, try the next
					opCode = -1
					break
				}
			}
			if opCode != -1 {
				// op code = v succesfully matched all ts[i's]
				opCodeMap[op] = append(opCodeMap[op], Instruction(opCode))
			}
		}
	}

	// Okay, unfortunantly (almost) every opcode matches to multiple instructions
	// Luckily, printing the possible op code to instructions, we get something like
	// 0: [1, 2]
	// 1: [2, 3, 4]
	// 2: [1, 3, 2, 4]
	// 3: [0]
	// 4: [2, 0, 3, 1, 4]
	// To find the final opcode -> instruction mapping we can continously choose the one where len == 1
	// and then remove the opcode from the other  arrays

	final := map[int]Instruction{}
	for len(final) < 16 {
		for i, ins := range opCodeMap {
			if len(ins) == 1 {
				final[i] = ins[0]
				opCodeMap = removeNums(opCodeMap, ins[0])
				break
			}
		}
	}

	regs := [4]int{}
	for i := stop; i < len(array); i++ {
		var in [4]int
		fmt.Sscanf(array[i], "%d %d %d %d", &in[0], &in[1], &in[2], &in[3])
		regs = execute(final[in[0]], in, regs)
	}

	return regs[0]
}

func remove(s []Instruction, i int) []Instruction {
	s[i] = s[len(s)-1]
	return s[:len(s)-1]
}

func removeNums(array [16][]Instruction, num uint8) [16][]Instruction {
	for i, v := range array {
		for j, t := range v {
			if t == num {
				array[i] = remove(v, j)
				break
			}
		}
	}
	return array
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
