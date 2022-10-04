package main

import "core:fmt"
import "core:os"
import "core:strings"
import "core:slice"
import "core:strconv"
import "core:time"
import "core:math"

read_file :: proc() -> []int 
{
	content, _ := os.read_entire_file_from_filename("input")	
	
	string := strings.clone_from(content)
	array := strings.split(string, "\n")

	to_int :: proc(s: string) -> int
	{
		num, ok := strconv.parse_i64(s)
		return cast(int)num
	}
	
	n := len(array) - 1;
	return slice.mapper(array[:n], to_int)
}

window :: 25

check_range :: proc(array: []int, target: int, start: int) -> bool
{
	end := start + window
	for i := start; i < end; i += 1
	{
		for j := i + 1; j < end; j += 1
		{
			if array[i] + array[j] == target
			{
				return true
			}
		}
	}
	
	return false
}

part_one :: proc(input: []int) -> int
{
	high := window
	
	for  i:= 0; ; i += 1
	{
		target := input[i + window]
		if !check_range(input, target, i)
		{
			return target
		}
	}
	
}

part_two :: proc(input: []int) -> int
{
	target := part_one(input)
	
	low := 0
	high := 1
	sum := input[low] + input[high]
	
	for sum != target
	{
		if sum < target
		{
			high += 1
			sum += input[high]
		}
		if sum > target
		{
			sum -= input[low]
			low += 1
		}

	}
	
	fin := input[low:high + 1]
	
	return slice.min(fin) + slice.max(fin)
}

timeFunc :: proc(f: proc($A)->$U, arg: A)
{
    sw : time.Stopwatch
    time.stopwatch_start(&sw)
    res := f(arg)
    time.stopwatch_stop(&sw)

    dur := time.stopwatch_duration(sw)
    duration := cast(u32)math.round(time.duration_milliseconds(dur))
    fmt.printf("(%dms)\t%v\n", duration, res)
}

main :: proc() 
{
	array := read_file()
	defer delete(array)
	
    timeFunc(part_one, array)
    timeFunc(part_two, array)
}
