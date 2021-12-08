package main

import (
	"fmt"
	"os"
	"regexp"
	"sort"
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

func RemoveCharacters(s, rem string) string {
	re, _ := regexp.Compile("[" + rem + "]")
	res := re.ReplaceAllString(s, "")
	if len(res) == 0 {
		panic("Empty result!")
	}
	return res
}

func Certain(s string) string {
	if len(s) > 1 {
		panic("Certain assignment assertion failed! Actual string: " + s)
	}
	return s
}

func AssignSignalsToSegments(signals []string) [7]string {
	var segments [7]string

	sort.Slice(signals, func(i, j int) bool { return len(signals[i]) < len(signals[j]) })
	segments[2], segments[5] = signals[0], signals[0]               // from segments of 1
	segments[0] = Certain(RemoveCharacters(signals[1], signals[0])) // from segments of 7
	segments[1] = RemoveCharacters(signals[2], signals[0])          // from segments of 4
	segments[3] = segments[1]

	signals_of_1_4_7 := signals[0] + signals[1] + signals[2]
	var segments_of_2, segments_of_5_or_3 string
	for i := 3; i <= 5; i++ {
		rem_segments := RemoveCharacters(signals[i], signals_of_1_4_7)
		if len(rem_segments) == 2 { // segments of 2
			segments_of_2 = signals[i]
		} else { // segments of 3 or 5
			segments_of_5_or_3 = signals[i]
		}
	}
	segments[1] = Certain(RemoveCharacters(segments[1], segments_of_2))
	segments[6] = Certain(RemoveCharacters(segments_of_5_or_3, signals_of_1_4_7))
	segments[4] = Certain(RemoveCharacters(RemoveCharacters(segments_of_2, signals_of_1_4_7), segments[6]))
	segments[3] = Certain(RemoveCharacters(segments[3], segments[1]))
	segments[2] = Certain(RemoveCharacters(segments[2], segments_of_2))
	segments[5] = Certain(RemoveCharacters(segments[5], segments[2]))

	return segments
}

func GetDigitFromSignals(signals string, segments [7]string) int {
	switch len := len(signals); len {
	case 2:
		return 1
	case 3:
		return 7
	case 4:
		return 4
	case 5: // 2, 3, 5
		if strings.Contains(signals, segments[4]) {
			return 2
		} else if strings.Contains(signals, segments[1]) {
			return 5
		} else {
			return 3
		}
	case 6: // 6, 9, 0
		if strings.Contains(signals, segments[3]) {
			if strings.Contains(signals, segments[4]) {
				return 6
			} else {
				return 9
			}
		} else {
			return 0
		}
	case 7:
		return 8
	default:
		panic("Invalid signals length!")
	}
}

func SolvePart2(entries []Entry) int {
	sum := 0
	for _, entry := range entries {
		number := 0
		segments := AssignSignalsToSegments(entry.signals)
		for _, output := range entry.outputs {
			digit := GetDigitFromSignals(output, segments)
			number *= 10
			number += digit
		}
		sum += number
	}

	return sum
}

func main() {
	filename := os.Args[1]
	input, err := os.ReadFile(filename)
	if err != nil {
		panic(err)
	}

	lines := strings.Split(string(input), "\n")
	entries := ReadEntries(lines)

	fmt.Println("Part 1:", SolvePart1(entries))
	fmt.Println("Part 2:", SolvePart2(entries))
}
