BUILD_DIR=build

.PHONY: clean

all: grammar interpreter

grammar: grammar.cf
	bnfc --haskell -m -d grammar.cf -o ${BUILD_DIR}
	cd ${BUILD_DIR} && make

interpreter: *.hs Typechecker/*.hs Interpreter/*.hs
	ghc Main.hs -package mtl -i${BUILD_DIR} -outputdir ${BUILD_DIR} -o interpreter

clean:
	rm -rf ${BUILD_DIR} interpreter
