package main

import (
	"bufio"
	"fmt"
	"os"
	// "strconv"
)

func main() {
	total_joltage := 0

	f, _ := os.Open("data/day-3-input")
	defer f.Close()

	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		bank := scanner.Text()
		bank_len := len(bank)
		max_index := bank_len
		max_digit := 0
		for i := bank_len - 2; i >= 0; i-- {
			digit := int(bank[i] - '0')
			if digit >= max_digit {
				max_digit = digit
				max_index = i
			}
		}
		line_joltage := max_digit * 10

		max_digit = 0
		for j := max_index + 1; j < bank_len; j++ {
			digit := int(bank[j] - '0')
			if digit >= max_digit {
				max_digit = digit
			}
		}
		line_joltage += max_digit
		total_joltage += line_joltage
	}

	fmt.Println(total_joltage)
}
