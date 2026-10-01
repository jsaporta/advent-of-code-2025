package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

func main() {
	var left_endpoints []int
	var right_endpoints []int
	var query_points []int

	f, _ := os.Open("data/day-5-input")
	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		row := scanner.Text()
		if strings.TrimSpace(row) == "" {
			break
		}
		row_split := strings.Split(row, "-")
		left, _ := strconv.Atoi(row_split[0])
		right, _ := strconv.Atoi(row_split[1])

		left_endpoints = append(left_endpoints, left)
		right_endpoints = append(right_endpoints, right)
	}
	for scanner.Scan() {
		row := strings.TrimSpace(scanner.Text())
		query_point, _ := strconv.Atoi(row)
		query_points = append(query_points, query_point)
	}

	var num_fresh int
	fresh := false
	for _, x := range query_points {
		for i := 0; i < len(left_endpoints); i++ {
			left_val := left_endpoints[i]
			right_val := right_endpoints[i]
			if left_val <= x && x <= right_val {
				fresh = true
			}
		}
		if fresh {
			num_fresh++
		}
		fresh = false
	}

	fmt.Println(num_fresh)
}
