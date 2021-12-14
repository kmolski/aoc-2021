package main

import (
	"fmt"
	"os"
	"sort"
	"strings"
)

type CharFreqs map[rune]int64
type CachedExpansion struct {
	freqs CharFreqs
	start byte
	end   byte
}

func ReadPairInsertions(part string) map[string]byte {
	insertions := map[string]byte{}
	for _, line := range strings.Split(part, "\n") {
		if len(line) > 0 {
			split := strings.Split(line, " -> ")
			insertions[split[0]] = split[1][0]
		}
	}
	return insertions
}

func MakeCacheKey(segment string, step int) string {
	return segment + fmt.Sprint(step) // e.g. AA36, BB8, CD21
}

func CharFrequencies(segment string) CharFreqs {
	if segment[0] == segment[1] {
		return CharFreqs{rune(segment[0]): 2}
	} else {
		return CharFreqs{rune(segment[0]): 1, rune(segment[1]): 1}
	}
}

func MergeCharFreqs(a, b CharFreqs) CharFreqs {
	mergedFreqs := CharFreqs{}
	for k, v := range a {
		mergedFreqs[k] = v
	}
	for k, v := range b {
		mergedFreqs[k] += v
	}
	return mergedFreqs
}

func Expand(segment string, insertions map[string]byte,
	cache map[string]CachedExpansion, step, maxSteps int) CharFreqs {

	cacheKey := MakeCacheKey(segment, step)
	if cachedFreqs, ok := cache[cacheKey]; ok {
		return cachedFreqs.freqs
	} else if insertion, ok := insertions[segment]; ok && step < maxSteps {
		segmentA := string(segment[0]) + string(insertion)
		segmentB := string(insertion) + string(segment[1])

		freqsA := Expand(segmentA, insertions, cache, step+1, maxSteps)
		freqsB := Expand(segmentB, insertions, cache, step+1, maxSteps)

		mergedFreqs := MergeCharFreqs(freqsA, freqsB)
		mergedFreqs[rune(insertion)] -= 1 // exclude common character of segmentA and segmentB
		cache[cacheKey] = CachedExpansion{mergedFreqs, segment[0], segment[1]}
		return mergedFreqs
	} else {
		freqs := CharFrequencies(segment)
		cache[cacheKey] = CachedExpansion{freqs, segment[0], segment[1]}
		return freqs
	}
}

func Solve(template string, insertions map[string]byte, maxSteps int) int64 {
	cache := map[string]CachedExpansion{}
	freqs := CharFreqs{}

	for i := 0; i < len(template)-1; i++ {
		window := template[i : i+2]
		expandedWindowFreqs := Expand(window, insertions, cache, 0, maxSteps)
		freqs = MergeCharFreqs(freqs, expandedWindowFreqs)
		freqs[rune(window[0])] -= 1 // exclude common character of segmentA and segmentB
	}

	counts := make([]int64, 0, len(freqs)) // create slice of element frequencies to sort
	for _, value := range freqs {
		counts = append(counts, value)
	}
	sort.Slice(counts, func(i, j int) bool { return counts[i] < counts[j] })
	return counts[len(counts)-1] - counts[0]
}

func main() {
	filename := os.Args[1]
	input, err := os.ReadFile(filename)
	if err != nil {
		panic(err)
	}

	parts := strings.Split(string(input), "\n\n")
	template, insertions := parts[0], ReadPairInsertions(parts[1])

	fmt.Println("Part 1:", Solve(template, insertions, 10))
	fmt.Println("Part 2:", Solve(template, insertions, 40))
}
