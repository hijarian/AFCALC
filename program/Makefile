SRCDIR=./src
TEMPDIR=obj
OUTDIR=bin/
COMPILER=ghc --make -i${SRCDIR} -outputdir ${TEMPDIR} -o ${OUTDIR}

all : 
	${COMPILER}afcalc src/Main.hs

simple :
	${COMPILER}afcalc-simple src/Simple.hs

testtheta :
	${COMPILER}test-theta src/TestTheta.hs

tests : 
	${COMPILER}afcalc-tests src/TestSimple.hs

