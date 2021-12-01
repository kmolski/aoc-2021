.PHONY: run
run: SonarSweep.php example.txt data.txt
	php SonarSweep.php example.txt
	php SonarSweep.php data.txt

.PHONY: test
test: SonarSweep.php example.txt example.out data.txt data.out
	php SonarSweep.php example.txt | diff - example.out
	php SonarSweep.php data.txt | diff - data.out
