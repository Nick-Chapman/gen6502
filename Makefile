
regression: the.out
	git diff the.out

the.out: src/*.hs
	stack run test > $@
