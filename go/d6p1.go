package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

func main() {
	ans := 0
	num_rows := 0
	var grid [][]int
	var ops []string

	f, _ := os.Open("data/day-6-input")
	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		row := strings.Fields(scanner.Text())

		if row[0] == "+" || row[0] == "*" {
			ops = row
		} else {
			int_row := make([]int, len(row))
			for i, s := range row {
				x, _ := strconv.Atoi(s)
				int_row[i] = x
			}
			grid = append(grid, int_row)
			num_rows += 1
		}
	}

	for j, op := range ops {
		var total int
		var nums []int

		for i := 0; i < num_rows; i++ {
			nums = append(nums, grid[i][j])
		}

		if op == "+" {
			total = 0
			for _, x := range nums {
				total += x
			}
		} else {
			total = 1
			for _, x := range nums {
				total *= x
			}
		}

		ans += total
	}

	fmt.Println(ans)
}
