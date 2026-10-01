package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
)

func main() {
	zero_cnt := 0
	curr_pos := 50

	f, err := os.Open("data/day-1-input")
	if err != nil {
		log.Fatal(err)
	}
	defer f.Close()

	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		instruction := scanner.Text()
		val, err := strconv.Atoi(instruction[1:len(instruction)])
		if err != nil {
			log.Fatal(err)
		}

		for val > 100 {
			zero_cnt += 1
			val -= 100
		}

		old_pos := curr_pos
		if instruction[0] == 'L' {
			curr_pos -= val
		} else {
			curr_pos += val
		}

		if curr_pos == 0 {
			zero_cnt += 1
		}
		if curr_pos < 0 {
			if old_pos > 0 {
				zero_cnt += 1
			}
			curr_pos += 100
		}
		if curr_pos >= 100 {
			zero_cnt += 1
			curr_pos -= 100
		}
	}
	fmt.Println(zero_cnt)
}
