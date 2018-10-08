.PHONY: clean top

main.native: main.ml model/*.ml
	ocamlbuild -use-ocamlfind main.native -I model

main.top: main.ml model/*.ml
	ocamlbuild -use-ocamlfind main.top -I model

top: main.top
	ledit ./main.top -I _build -I _build/model -open Sail_lib -open Riscv -open Generators

clean:
	ocamlbuild -clean
