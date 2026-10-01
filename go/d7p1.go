package main

import (
	"bufio"
	"fmt"
	"os"
)

func main() {
	var curr []byte
	var num_splits int

	f, _ := os.Open("data/day-7-input")
	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		var row string = scanner.Text()
		if curr == nil {
			curr = []byte(row)
			continue
		}

		var row_len int = len(row)

		for i := 1; i < row_len - 1; i ++ {
			if curr[i] == 'S' {
				curr[i] = '|'
			} else if curr[i] == '|' {
				if row[i] == '^' {
					curr[i - 1] = '|'
					curr[i] = '^'
					curr[i + 1] = '|'
					num_splits++
				} else {
					curr[i] = '|'
				}
			}
		}
	}

	fmt.Println(num_splits)
}
