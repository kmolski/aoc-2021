.PHONY: run test
run: DiracDice.pl; swipl DiracDice.pl

test: DiracDice.pl data.out
	swipl DiracDice.pl | diff - data.out
