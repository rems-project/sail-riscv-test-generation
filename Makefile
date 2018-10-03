.PHONY: main.native clean

main.native:
	ocamlbuild -use-ocamlfind main.native -I model

clean:
	ocamlbuild -clean
