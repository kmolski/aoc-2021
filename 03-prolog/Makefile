.PHONY: run
run: BinaryDiagnostic.pl example.txt data.txt
	swipl BinaryDiagnostic.pl example.txt
	swipl BinaryDiagnostic.pl data.txt

.PHONY: test
test: BinaryDiagnostic.pl example.txt example.out data.txt data.out
	swipl BinaryDiagnostic.pl example.txt | diff - example.out
	swipl BinaryDiagnostic.pl data.txt | diff - data.out
