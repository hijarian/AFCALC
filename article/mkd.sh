#!/bin/bash
mv programming.tex tempprog.tex
sed -e 's/verbatim/lstlisting/; s/verb!/lstinline!/g' tempprog.tex > programming.tex
latex Диплом
bibtex8 -B -c cp1251 Диплом
latex Диплом
latex Диплом
mv tempprog.tex programming.tex
#dvipdfm -r 1200 Диплом.dvi

