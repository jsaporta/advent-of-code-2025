package main

import (
	"bufio"
	"fmt"
	"os"
)

func scan_left(left_bound, right_bound int, bank string) (max_digit, max_index int) {
	for i := right_bound; i >= left_bound; i-- {
		digit := int(bank[i] - '0')
		if digit >= max_digit {
			max_digit = digit
			max_index = i
		}
	}
	return
}

func scan_right(left_bound, right_bound int, bank string) (max_digit, max_index int) {
	for i := left_bound; i <= right_bound; i++ {
		digit := int(bank[i] - '0')
		if digit >= max_digit {
			max_digit = digit
			max_index = i
		}
	}
	return
}

func main() {
	total_joltage := 0

	f, _ := os.Open("data/day-3-input")
	defer f.Close()

	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		bank := scanner.Text()
		bank_len := len(bank)
		var max_digit int
		var line_joltage int
		var K int = 12
		max_index := -1

		for i := 0; i < K - 1; i++ {
			max_digit, max_index = scan_left(max_index + 1, bank_len - K + i,
							 bank)
			line_joltage *= 10
			line_joltage += max_digit
		}


		max_digit, max_index = scan_right(max_index + 1,
						  bank_len - 1, bank)
		line_joltage *= 10
		line_joltage += max_digit
		total_joltage += line_joltage
	}

	fmt.Println(total_joltage)
}
