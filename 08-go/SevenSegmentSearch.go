package main

import (
	"fmt"
	"os"
	"strings"
)

type Entry struct {
	signals []string
	outputs []string
}

func ReadEntries(lines []string) []Entry {
	var entries []Entry
	for _, line := range lines {
		if len(line) > 0 {
			split := strings.Split(line, " | ")
			entry := Entry{signals: strings.Split(split[0], " "), outputs: strings.Split(split[1], " ")}
			entries = append(entries, entry)
		}
	}

	return entries
}

func SolvePart1(entries []Entry) int {
	count := 0
	for _, entry := range entries {
		for _, signal := range entry.outputs {
			len := len(signal)
			if len == 2 || len == 3 || len == 4 || len == 7 {
				count += 1
			}
		}
	}

	return count
}

func main() {
	filename := os.Args[1]
	input, err := os.ReadFile(filename)
	if err != nil {
		panic(err)
	}

	lines := strings.Split(string(input), "\n")
	entries := ReadEntries(lines)

	fmt.Println("Part 1: ", SolvePart1(entries))
}
