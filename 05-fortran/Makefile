.PHONY: run
run: a.out example.txt data.txt
	./a.out example.txt
	./a.out data.txt

.PHONY: test
test: a.out example.txt example.out data.txt data.out
	./a.out example.txt | diff - example.out
	./a.out data.txt | diff - data.out

a.out: HydrothermalVenture.f
	gfortran10 HydrothermalVenture.f
