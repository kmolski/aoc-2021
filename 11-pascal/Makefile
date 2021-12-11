.PHONY: run test
run: DumboOctopus example.txt data.txt; ./DumboOctopus example.txt; ./DumboOctopus data.txt

test: DumboOctopus example.txt example.out data.txt data.out
	./DumboOctopus example.txt | diff - example.out
	./DumboOctopus data.txt | diff - data.out

DumboOctopus: DumboOctopus.pas; fpc DumboOctopus.pas
