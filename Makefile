.PHONY: clean top

main.native:
	ocamlbuild -use-ocamlfind main.native -I model

main.top:
	ocamlbuild -use-ocamlfind main.top -I model

top: main.top
	ledit ./main.top -I _build -I _build/model -open Sail_lib -open Riscv

clean:
	ocamlbuild -clean
