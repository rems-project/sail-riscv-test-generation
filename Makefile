.PHONY: default clean top main.native main.top

SAIL_RISCV_DIR=../../riscv-implementations/sail-riscv

default: main.native

get_model:
	make -C $(SAIL_RISCV_DIR) generated_definitions/ocaml/riscv-ast.ml
	cp $(SAIL_RISCV_DIR)/generated_definitions/ocaml/riscv-ast.ml model/riscv.ml

main.native:
	ocamlbuild -use-ocamlfind main.native -I model

main.top:
	ocamlbuild -use-ocamlfind main.top -I model

top: main.top
	ledit ./main.top -I _build -I _build/model -open Sail_lib -open Riscv -open Generators

clean:
	ocamlbuild -clean
