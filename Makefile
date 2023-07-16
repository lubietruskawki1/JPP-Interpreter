BUILD_DIR=build

BNFC=bnfc

.PHONY: clean

all: grammar interpreter

grammar: Elina.cf
	${BNFC} --haskell --functor -m -d Elina.cf -o ${BUILD_DIR}
	make -C ${BUILD_DIR}

interpreter: *.hs
	ghc Main.hs -package mtl -i${BUILD_DIR} -outputdir ${BUILD_DIR} -o interpreter

clean:
	rm -rf ${BUILD_DIR} interpreter