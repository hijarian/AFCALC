# Directory with sources
SRCDIR=./src
# Directory to hold intermediate files
TEMPDIR=obj
# Directory to hold binaries
OUTDIR=bin/
# Main build invocation string
COMPILER=ghc --make -i${SRCDIR} -outputdir ${TEMPDIR} -o ${OUTDIR}

all :
	${COMPILER}graph src/Main.hs

# Clean built objects
clean :
	rm -rf obj/* bin/*

# Remove everything leaving only sources
purge : clean
	rm -rf img/*
