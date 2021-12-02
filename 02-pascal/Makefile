.PHONY: run
run: Dive example.txt data.txt
	./Dive example.txt
	./Dive data.txt

.PHONY: test
test: Dive example.txt example.out data.txt data.out
	./Dive example.txt | diff - example.out
	./Dive data.txt | diff - data.out

Dive: Dive.pas
	fpc Dive.pas
