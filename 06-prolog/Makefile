.PHONY: run
run: Lanternfish.pl example.txt data.txt
	swipl Lanternfish.pl example.txt
	swipl Lanternfish.pl data.txt

.PHONY: test
test: Lanternfish.pl example.txt example.out data.txt data.out
	swipl Lanternfish.pl example.txt | diff - example.out
	swipl Lanternfish.pl data.txt | diff - data.out
