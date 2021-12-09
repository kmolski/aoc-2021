.PHONY: run test
run: SmokeBasin example.txt data.txt; ./SmokeBasin example.txt; ./SmokeBasin data.txt

test: SmokeBasin example.txt example.out data.txt data.out
	./SmokeBasin example.txt | diff - example.out
	./SmokeBasin data.txt | diff - data.out

SmokeBasin: SmokeBasin.pas; fpc SmokeBasin.pas
