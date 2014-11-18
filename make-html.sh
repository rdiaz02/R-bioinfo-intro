#!/bin/bash
FILE=$1
BASENAME=$(basename $FILE .Rnw)
RSCRIPT="/usr/bin/Rscript"
BASENAME2=$BASENAME-knitr-html
FILE2=$BASENAME2.Rnw

cp $FILE $FILE2
sed -i 's/^%%listings-knitr-html%%//' $FILE2
# but nameref does not work either
sed -i 's/\\nameref{/\\ref{/' $FILE2
$RSCRIPT -e 'library(knitr); knit("'$FILE2'")'

# do not use left overs
rm $BASENAME2.aux
# make sure we have a proper aux, etc
texi2pdf $BASENAME2.tex
sweave2html $BASENAME2
mv $BASENAME2.html $BASENAME.html

mkdir $BASENAME-html-dir
cp $BASENAME.html ./$BASENAME-html-dir/.
cp -a figure ./$BASENAME-html-dir/.
zip -r $BASENAME-html-dir.zip $BASENAME-html-dir

## sweave2html from http://biostat.mc.vanderbilt.edu/wiki/Main/SweaveConvert#Converting_from_LaTeX_to_html  
