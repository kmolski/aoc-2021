package main

import (
	"encoding/json"
	"fmt"
	"math"
	"os"
	"strings"
)

type NumberNode struct {
	left  *NumberNode
	right *NumberNode
	value int
}

func ParseTokenTree(tokenTree []interface{}) NumberNode {
	if len(tokenTree) != 2 {
		panic("Expected a pair of 2 elements!")
	}

	newNode := NumberNode{nil, nil, 0}
	switch leftValue := tokenTree[0].(type) {
	case float64: // JSON number
		newNode.left = &NumberNode{nil, nil, int(leftValue)}
	case []interface{}: // child node
		leftNode := ParseTokenTree(leftValue)
		newNode.left = &leftNode
	}
	switch rightValue := tokenTree[1].(type) {
	case float64: // JSON number
		newNode.right = &NumberNode{nil, nil, int(rightValue)}
	case []interface{}: // child node
		rightNode := ParseTokenTree(rightValue)
		newNode.right = &rightNode
	}

	return newNode
}

func ParseNumbers(input []byte) []NumberNode {
	numbers := make([]NumberNode, 0)
	trimmed := strings.TrimSpace(string(input))
	for _, line := range strings.Split(trimmed, "\n") {
		var tokenTree []interface{}
		json.Unmarshal([]byte(line), &tokenTree)

		newNumber := ParseTokenTree(tokenTree)
		numbers = append(numbers, newNumber)
	}

	return numbers
}

func CalculateMagnitude(number *NumberNode) (ret int) {
	if number.left == nil && number.right == nil { // leaf node
		return number.value
	} else {
		return CalculateMagnitude(number.left)*3 + CalculateMagnitude(number.right)*2
	}
}

func AddToClosestLeafLeft(number NumberNode, value int) NumberNode {
	if number.left == nil && number.right == nil { // leaf node
		return NumberNode{nil, nil, number.value + value}
	} else {
		newRight := AddToClosestLeafLeft(*number.right, value)
		return NumberNode{number.left, &newRight, 0}
	}
}

func AddToClosestLeafRight(number NumberNode, value int) NumberNode {
	if number.left == nil && number.right == nil { // leaf node
		return NumberNode{nil, nil, number.value + value}
	} else {
		newLeft := AddToClosestLeafRight(*number.left, value)
		return NumberNode{&newLeft, number.right, 0}
	}
}

func ExplodeNested(number NumberNode, nestLevel int) (NumberNode, bool, int, int) {
	if number.left == nil && number.right == nil {
		return number, false, 0, 0
	} else if nestLevel == 4 {
		left := number.left.value
		right := number.right.value

		number.left = nil
		number.right = nil
		number.value = 0

		return number, true, left, right
	} else {
		newLeft, exploded, left, right := ExplodeNested(*number.left, nestLevel+1)
		if exploded {
			newRight := AddToClosestLeafRight(*number.right, right)
			return NumberNode{&newLeft, &newRight, 0}, exploded, left, 0
		}

		newRight, exploded, left, right := ExplodeNested(*number.right, nestLevel+1)
		if exploded {
			newLeft := AddToClosestLeafLeft(*number.left, left)
			return NumberNode{&newLeft, &newRight, 0}, exploded, 0, right
		}
		return number, exploded, 0, 0
	}
}

func SplitBigLeaf(number NumberNode) (NumberNode, bool) {
	if number.left == nil && number.right == nil {
		if number.value >= 10 {
			value := float64(number.value)
			number.left = &NumberNode{nil, nil, int(math.Floor(value / 2))}
			number.right = &NumberNode{nil, nil, int(math.Ceil(value / 2))}
			number.value = 0
			return number, true
		} else {
			return number, false
		}
	} else {
		newLeft, split := SplitBigLeaf(*number.left)
		number.left = &newLeft
		if split {
			return number, split
		}

		newRight, split := SplitBigLeaf(*number.right)
		number.right = &newRight
		return number, split
	}
}

func ReduceNumber(number NumberNode) NumberNode {
	for {
		if exploded, didExplode, _, _ := ExplodeNested(number, 0); didExplode {
			number = exploded
			continue
		}

		if split, didSplit := SplitBigLeaf(number); didSplit {
			number = split
			continue
		}

		return number
	}
}

func SumNumbers(numbers []NumberNode) NumberNode {
	sum := numbers[0]
	for _, addend := range numbers[1:] {
		left := sum
		right := addend
		temp := NumberNode{&left, &right, 0}
		sum = ReduceNumber(temp)
	}
	return sum
}

func SolvePart1(numbers []NumberNode) int {
	sum := SumNumbers(numbers)
	return CalculateMagnitude(&sum)
}

func SolvePart2(numbers []NumberNode) int {
	max := 0
	for i := 0; i < len(numbers); i++ {
		for j := 0; j < len(numbers); j++ {
			if i != j {
				left := numbers[i]
				right := numbers[j]
				temp := NumberNode{&left, &right, 0}
				sum := ReduceNumber(temp)

				magnitude := CalculateMagnitude(&sum)
				if magnitude > max {
					max = magnitude
				}
			}
		}
	}

	return max
}

func main() {
	filename := os.Args[1]
	input, err := os.ReadFile(filename)
	if err != nil {
		panic(err)
	}

	numbers := ParseNumbers(input)
	fmt.Println("Part 1:", SolvePart1(numbers))
	fmt.Println("Part 2:", SolvePart2(numbers))
}
