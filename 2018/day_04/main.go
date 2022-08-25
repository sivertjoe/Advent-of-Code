package main

import (
	"bufio"
	"fmt"
	"os"
	"sort"
	"strconv"
	"strings"
	"time"
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
func parse(line string) guardInfo {

	var year, month, day, hour, minute int
	idx := strings.Index(line, "]")
	info := line[idx+2:]

	fmt.Sscanf(line, "[%d-%d-%d %d:%d]", &year, &month, &day, &hour, &minute)
	time := time.Date(year, time.Month(month), day, hour, minute, 0, 0, time.UTC)

	return guardInfo{time: time, info: info}
}

type guardInfo struct {
	time time.Time
	info string
}

// [1518-09-08 23:47] Guard #2437 begins shift
// [1518-05-14 00:53] wakes up
// [1518-06-25 00:02] falls asleep

func partOne(array []string) int {
	sort.Strings(array)
	var guards []guardInfo
	for _, line := range array {
		guards = append(guards, parse(line))
	}

	m := make(map[int]int)
	m2 := map[int]map[int]int{}

	var t0 time.Time
	var gid int

	for _, line := range array {
		guard := parse(line)
		split := strings.Split(guard.info, " ")

		switch split[0] {
		case "Guard":
			gid, _ = strconv.Atoi(split[1][1:])
			if _, ok := m2[gid]; !ok {
				m2[gid] = map[int]int{}
			}
		case "falls":
			t0 = guard.time

		case "wakes":
			for i := t0.Minute(); i != guard.time.Minute(); i = (i + 1) % 60 {
				m2[gid][i] += 1
				m[gid] += 1
			}
		}
	}

	max := 0
	id := 0

	for gid, time := range m {
		if time > max {
			id = gid
			max = time
		}
	}

	mm := 0
	mmid := 0
	for i, v := range m2[id] {
		if v > mm {
			mm = v
			mmid = i
		}
	}

	return id * mmid
}

func partTwo(array []string) int {
	sort.Strings(array)
	var guards []guardInfo
	for _, line := range array {
		guards = append(guards, parse(line))
	}

	m2 := map[int]map[int]int{}

	maxMin := 0
	maxMinMin := 0
	maxMinId := 0

	var t0 time.Time
	var gid int

	for _, line := range array {
		guard := parse(line)
		split := strings.Split(guard.info, " ")

		switch split[0] {
		case "Guard":
			gid, _ = strconv.Atoi(split[1][1:])
			if _, ok := m2[gid]; !ok {
				m2[gid] = map[int]int{}
			}
		case "falls":
			t0 = guard.time

		case "wakes":
			for i := t0.Minute(); i != guard.time.Minute(); i = (i + 1) % 60 {
				m2[gid][i] += 1
				if m2[gid][i] > maxMin {
					maxMin = m2[gid][i]
					maxMinId = gid
					maxMinMin = i
				}
			}
		}
	}
	return maxMinMin * maxMinId
}

func main() {
	array := readLine("input")
	fmt.Println(partOne(array))
	fmt.Println(partTwo(array))
}
