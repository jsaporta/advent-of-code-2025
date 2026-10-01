package main

import (
	"bufio"
	"fmt"
	"math"
	"math/big"
	"os"
)

func to_val(arr []int) (ans int) {
	arr_len := len(arr)
	for i, x := range arr {
		ans += int(math.Pow(float64(10), float64(arr_len - i - 1))) * x
	}
	return
}

func sum(arr []int) (ans int) {
	for _, x := range arr {
		ans += x
	}
	return
}

func main() {
	ans := big.NewInt(0)
	var grid []string

	f, _ := os.Open("data/day-6-input")
	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		row := scanner.Text()

		grid = append(grid, row)
	}

	// for each column, working backwards
	var vals []int

	for j := len(grid[0]) - 1; j >= 0; j-- {
		var col []int
		// for each row
		for i := 0; i < len(grid) - 1; i++ {
			if grid[i][j] != ' ' {
				col = append(col, int(grid[i][j] - '0'))
			}
		}
		if len(col) != 0 {
			vals = append(vals, to_val(col))
		}

		var op byte = grid[len(grid) - 1][j]
		if op == '+' {
			total := big.NewInt(0)
			for _, x := range vals {
				total.Add(total, big.NewInt(int64(x)))
			}
			ans.Add(ans, total)
			vals = []int{}
		}
		if op == '*' {
			total := big.NewInt(1)
			for _, x := range vals {
				total.Mul(total, big.NewInt(int64(x)))
			}
			ans.Add(ans, total)
			vals = []int{}
		}
	}

	fmt.Println(ans)
}
