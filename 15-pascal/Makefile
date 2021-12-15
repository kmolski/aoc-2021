.PHONY: run test
run: Chiton example.txt data.txt; ./Chiton example.txt; ./Chiton data.txt

test: Chiton example.txt example.out data.txt data.out
	./Chiton example.txt | diff - example.out
	./Chiton data.txt | diff - data.out

Chiton: Chiton.pas; fpc Chiton.pas
