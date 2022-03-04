package main

import (
	"fmt"
	"os"
	"strings"
)

func CharToPixel(ch rune) bool {
	return ch == '#'
}

func ParseInput(input []byte) ([]bool, [][]bool) {
	parts := strings.Split(string(input), "\n\n")

	replacements := make([]bool, len(parts[0]))
	for i, ch := range strings.TrimSpace(parts[0]) {
		replacements[i] = CharToPixel(ch)
	}

	lines := strings.Split(strings.TrimSpace(parts[1]), "\n")
	image := make([][]bool, len(lines))
	for i, l := range lines {
		row := make([]bool, len(l))
		for j, ch := range l {
			row[j] = CharToPixel(ch)
		}
		image[i] = row
	}

	return replacements, image
}

func CountLitPixels(image [][]bool) int {
	count := 0
	for _, row := range image {
		for _, px := range row {
			if px {
				count += 1
			}
		}
	}
	return count
}

func GetNewPixel(replacements []bool, image [][]bool, farPixels bool, x, y int) bool {
	index := 0
	updateIndex := func(i, j int) {
		index |= 0b100_000_000 >> ((i+1)*3 + j + 1) // -1..1 offsets to 0..2
	}

	maxY, maxX := len(image), len(image[0])
	for i := -1; i <= 1; i++ {
		for j := -1; j <= 1; j++ {
			ny, nx := y+i, x+j
			if (0 <= ny && ny < maxY) && (0 <= nx && nx < maxX) {
				if image[ny][nx] {
					updateIndex(i, j)
				}
			} else if farPixels { // for pixels outside the image
				updateIndex(i, j)
			}
		}
	}
	return replacements[index]
}

func Solve(replacements []bool, image [][]bool, n int) int {
	farPixels := false
	for i := 0; i < n; i++ {
		newImage := make([][]bool, len(image)+2) // enlarge the image by 2 rows
		for y := range newImage {
			newRow := make([]bool, len(image[0])+2) // and 2 columns
			for x := range newRow {
				// offset the newImage into the lower-right direction by (1, 1)
				newRow[x] = GetNewPixel(replacements, image, farPixels, x-1, y-1)
			}
			newImage[y] = newRow
		}
		image = newImage

		if farPixels {
			farPixels = replacements[511]
		} else {
			farPixels = replacements[0]
		}
	}
	return CountLitPixels(image)
}

func main() {
	filename := os.Args[1]
	input, err := os.ReadFile(filename)
	if err != nil {
		panic(err)
	}

	replacements, image := ParseInput(input)
	fmt.Println("Part 1:", Solve(replacements, image, 2))
	fmt.Println("Part 2:", Solve(replacements, image, 50))
}
