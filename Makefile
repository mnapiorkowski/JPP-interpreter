.PHONY: clean

all: grammar

grammar: grammar.cf
	bnfc --haskell -d grammar.cf

clean:
	rm -rf Grammar
