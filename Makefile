OCB = ocamlbuild -j 8 -use-ocamlfind -use-menhir

.PHONY: all native clean

all: native

clean:
	$(OCB) -clean

native:
	$(OCB) main.native

test:
	cat ./test.j | ./main.native
