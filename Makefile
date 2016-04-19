UNAME := $(shell uname)
ifeq ($(UNAME), Linux)
  FORMAT=elf64
else
ifeq ($(UNAME), Darwin)
  FORMAT=macho64
endif
endif

PKGS=oUnit,extlib,unix,ocamlgraph
BUILD=ocamlbuild -r -use-ocamlfind

NUMREGS=8

main: main.ml compile.ml runner.ml expr.ml instruction.ml parser.mly lexer.mll color.ml
	$(BUILD) -package $(PKGS) main.native
	mv main.native main

test: compile.ml runner.ml test.ml expr.ml instruction.ml parser.mly lexer.mll color.ml
	$(BUILD) -package $(PKGS) test.native
	mv test.native test

output/%.run: output/%.o main64.c
	clang -mstackrealign -g -o $@ main64.c $<

output/%.o: output/%.s
	nasm -f $(FORMAT) -o $@ $<

output/%.s: input/%.diamond main
	./main $< $(NUMREGS) > $@

clean:
	rm -rf output/*.o output/*.s output/*.dSYM output/*.run *.log
	rm -rf _build/
	rm -f main test

