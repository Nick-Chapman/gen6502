
top: dev-collatz

ocaml-collatz:
	dune exec examples/main.exe

test: regression

dev-%: src/*.hs examples/%.ml6
	stack run dev examples/$*.ml6

regression: the.out
	git diff the.out

the.out: src/*.hs
	stack run test > $@
