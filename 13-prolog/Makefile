.PHONY: run test
run: TransparentOrigami.pl example.txt data.txt; swipl TransparentOrigami.pl example.txt; swipl TransparentOrigami.pl data.txt

test: TransparentOrigami.pl example.txt example.out data.txt data.out
	swipl TransparentOrigami.pl example.txt | diff - example.out
	swipl TransparentOrigami.pl data.txt | diff - data.out
