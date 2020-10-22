MODULES=game player tile test_game
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind

default: build
	utop

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST)

build:
	$(OCAMLBUILD) $(OBJECTS)
	