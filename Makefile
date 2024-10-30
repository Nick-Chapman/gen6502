
top: regression

ocaml-exec:
	dune exec examples/main.exe

regression: the.out
	git diff the.out

the.out: src/*.hs examples/examples.ml6
	stack run > $@
