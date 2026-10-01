package main

import (
	"bufio"
	"fmt"
	"os"
)

func main() {
	var prev []int
	var num_timelines int = 1

	f, _ := os.Open("data/day-7-input")
	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		var row string = scanner.Text()
		if prev == nil {
			prev = make([]int, len(row))
			for i := 0; i < len(row); i++ {
				if row[i] == 'S' {
					prev[i] = 1
				} else {
					prev[i] = 0
				}
			}
			continue
		}

		var row_len int = len(row)
		curr := make([]int, row_len)

		for i := 0; i < row_len; i ++ {
			if prev[i] > 0 {
				if row[i] == '^' {
					if i >= 1 {
						curr[i - 1] += prev[i]
					}
					// curr[i] = 0
					if i <= row_len - 1 {
						curr[i + 1] += prev[i]
					}
					num_timelines += prev[i]
				} else {
					curr[i] += prev[i]
				}
			}
		}
		prev = curr
	}

	fmt.Println(num_timelines)
}
