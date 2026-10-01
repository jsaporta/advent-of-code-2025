package main

import (
	"bufio"
	"fmt"
	"os"
)

func can_access(i, j int, grid []string) bool {
	i_vals := []int{i - 1, i, i + 1}
	j_vals := []int{j - 1, j, j + 1}
	num_rolls := 0
	for _, r := range i_vals {
		for _, c := range j_vals {
			if r < 0 || r >= len(grid) || c < 0 || c >= len(grid[0]) {
				continue
			} else if r == i && c == j {
				if grid[i][j] != '@' {
					return false
			}
			} else if grid[r][c] == '@' {
				num_rolls += 1
			}

			if num_rolls >= 4 {
				return false
			}
		}
	}
	return true
}

func main() {
	var total_num_rolls int;

	// grid := make([]string, 8)
	var grid []string;

	f, _ := os.Open("data/day-4-input")
	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		row := scanner.Text()
		grid = append(grid, row)
	}
	for i:= 0; i < len(grid); i++ {
		for j := 0; j < len(grid[0]); j++ {
			if can_access(i, j, grid) {
				total_num_rolls++
			}
		}
	}

	fmt.Println(total_num_rolls)
}
