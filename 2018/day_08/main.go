package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"
)

func readLine(path string) []string {
	b, _ := os.ReadFile(path)
	str := string(b)
	return strings.Split(str, "\n")
}

type node struct {
	numChilds   int
	numMetadata int

	children []node
	metadata []int
}

func parse(array []int, idx int) (node, int) {
	numChilds := array[idx]
	numMetadata := array[idx+1]

	children := []node{}

	idx += 2
	stop := idx + numChilds
	for i := idx; i < stop; i++ {

		node, next := parse(array, idx)
		idx = next
		children = append(children, node)
	}

	metadata := []int{}
	for i := idx; i < idx+numMetadata; i++ {
		metadata = append(metadata, array[i])
	}
	idx += numMetadata

	return node{numChilds: numChilds, numMetadata: numMetadata, children: children, metadata: metadata}, idx
}

func partOne(array []int) int {
	node, _ := parse(array, 0)
	return count(node)
}

func count(node node) int {
	sum := 0
	for _, m := range node.metadata {
		sum += m
	}

	for _, n := range node.children {
		sum += count(n)
	}
	return sum
}

func value(node node) int {
	sum := 0
	if node.numChilds == 0 {
		for _, v := range node.metadata {
			sum += v
		}
	} else {
		for _, idx := range node.metadata {
			if idx > 0 && idx <= node.numChilds {
				sum += value(node.children[idx-1])
			}
		}
	}
	return sum

}

func partTwo(array []int) int {
	node, _ := parse(array, 0)
	return value(node)
}

func getNums(array []string) []int {
	nums := []int{}
	for _, line := range array {
		spl := strings.Split(line, " ")
		for _, num := range spl {
			n, err := strconv.Atoi(num)
			if err == nil {
				nums = append(nums, n)
			}
		}
	}
	return nums
}

func main() {
	args := os.Args
	if len(args) != 2 {
		fmt.Println("Usage: ./main <input file>")
		os.Exit(-1)
	}

	nums := getNums(readLine(args[1]))

	fmt.Println(partOne(nums))
	fmt.Println(partTwo(nums))
}
