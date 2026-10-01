package main

import (
	"bufio"
	"cmp"
	"fmt"
	"os"
	"slices"
	"strconv"
	"strings"
)

func main() {
	var intervals [][]int

	f, _ := os.Open("data/day-5-input")
	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		row := scanner.Text()
		if strings.TrimSpace(row) == "" {
			break
		}
		row_split := strings.Split(row, "-")
		left, _ := strconv.Atoi(row_split[0])
		right, _ := strconv.Atoi(row_split[1])

		intervals = append(intervals, []int{left, right})
	}

	intervalCompare := func(x, y []int) int {
		c := cmp.Compare(x[0], y[0])
		if c == 0 {
			return cmp.Compare(x[1], y[1])
		} else {
			return c
		}
	}

	intervalSort := func(intervals [][]int) {
		slices.SortFunc(intervals, intervalCompare)
	}

	intervalSort(intervals)

	var merged_intervals [][]int
	curr := intervals[0]
	for _, x := range intervals {
		if x[0] <= curr[1] {
			curr[1] = max(curr[1], x[1])
		} else {
			merged_intervals = append(merged_intervals, curr)
			curr = x
		}
	}
	merged_intervals = append(merged_intervals, curr)

	num_fresh := 0
	for _, x := range merged_intervals {
		num_fresh += x[1] - x[0] + 1
	}

	fmt.Println(num_fresh)
}
