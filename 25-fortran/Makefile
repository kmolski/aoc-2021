.PHONY: run test
run: a.out example.txt data.txt; ./a.out example.txt; ./a.out data.txt

test: a.out example.txt example.out data.txt data.out
	./a.out example.txt | diff - example.out
	./a.out data.txt | diff - data.out

a.out: SeaCucumber.f; gfortran10 SeaCucumber.f
