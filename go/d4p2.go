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
			} else if grid[r][c] == '@' || grid[r][c] == 'x' {
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
	diff := 1
	var total_num_rolls int;

	// grid := make([]string, 8)
	var grid []string;

	f, _ := os.Open("data/day-4-input")
	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		row := scanner.Text()
		grid = append(grid, row)
	}

	for diff > 0 {
		diff = 0
		for i:= 0; i < len(grid); i++ {
			for j := 0; j < len(grid[0]); j++ {
				if can_access(i, j, grid) {
					grid[i] = grid[i][:j] + "x" + grid[i][(j + 1):]
					diff++
				}
			}
		}
		for i:= 0; i < len(grid); i++ {
			for j := 0; j < len(grid[0]); j++ {
				if grid[i][j] == 'x' {
					grid[i] = grid[i][:j] + "." + grid[i][(j + 1):]
				}
			}
		}
		total_num_rolls += diff
	}

	fmt.Println(total_num_rolls)
}
