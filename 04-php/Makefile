.PHONY: run
run: GiantSquid.php example.txt data.txt
	php GiantSquid.php example.txt
	php GiantSquid.php data.txt

.PHONY: test
test: GiantSquid.php example.txt example.out data.txt data.out
	php GiantSquid.php example.txt | diff - example.out
	php GiantSquid.php data.txt | diff - data.out
