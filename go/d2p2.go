package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"
)

func isValid(s string) (valid bool) {
	valid = true
	len_s := len(s)
	for i := 1; i <= (len_s / 2) + 1; i++ {
		if len_s % i == 0 && s == strings.Repeat(s[:i], len_s / i) {
			valid = false
			break
		}
	}
	return
}

func main() {
	ans := 0

	f, _ := os.ReadFile("data/day-2-input")
	for _, s := range strings.Split(string(f), ",") {
		first_sec := strings.Split(strings.TrimSpace(s), "-")
		first, _ := strconv.Atoi(first_sec[0])
		second, _ := strconv.Atoi(first_sec[1])

		for x := first; x <= second; x++ {
			if !isValid(strconv.Itoa(x)) {
				ans += x
			}
		}
	}

	fmt.Println(ans)
}
