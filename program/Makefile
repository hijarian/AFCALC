# Directory with sources
SRCDIR=./src
# Directory to hold intermediate files
TEMPDIR=obj
# Directory to hold binaries
OUTDIR=bin/
# Main build invocation string
COMPILER=ghc --make -i${SRCDIR} -outputdir ${TEMPDIR} -o ${OUTDIR}

# TODO: I wanna be able to produce three different executables: one with interactive session, one with mass-produced PNG charts covering some set of parameter values and one with analytical tests about model
# For now there is only a executable for mass-producing charts

# Processing of Main.hs in case I will ever use generic names for any models
all : 
	${COMPILER}afcalc src/Main.hs

# Processing of BlastModel.Model_1975_Kotlyar
kotlyar :
	${COMPILER}afcalc_kotlyar src/TestKotlyar.hs

# Toy target for demonstration of plotting charts
exponent :
	${COMPILER}afcalc_exponent src/TestExponent.hs

# Clean built objects
clean : 
	rm -rf obj/* bin/*

# Remove everything leaving only sources
purge : clean
	rm -rf img/*

# Test suite
# TODO: Incomplete test suite!
#tests :
# Tests of Theta module. For now works only in ghci
#	${COMPILER}test-theta src/TestTheta.hs 

