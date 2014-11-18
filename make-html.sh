#!/bin/bash
FILE=$1
BASENAME=$(basename $FILE .Rnw)
RSCRIPT="/usr/bin/Rscript"
BASENAME2=$BASENAME-knitr-html
FILE2=$BASENAME2.Rnw

cp $FILE $FILE2
sed -i 's/^%%listings-knitr-html%%//' $FILE2
sed -i 's/^##listings-knitr-html%%//' $FILE2

# but nameref does not work either
sed -i 's/\\nameref{/\\ref{/' $FILE2

# and CRNApkg and Rfunction and others confuse it

sed -i 's/\\CRANpkg{/\\texttt{/' $FILE2
sed -i 's/\\Rfunction{/\\texttt{/' $FILE2
sed -i 's/\\Robject{/\\texttt{/' $FILE2
## sed -i 's/\\CRANpkg{/\\texttt{/' $FILE2

## make sure things are clean
rm figures_html/*.pdf
rm figures_html/*.png

$RSCRIPT -e 'library(knitr); knit("'$FILE2'")'

# do not use left overs
rm $BASENAME2.aux
# make sure we have a proper aux, etc
texi2pdf $BASENAME2.tex
sweave2html $BASENAME2
mv $BASENAME2.html $BASENAME.html

rm $BASENAME-html-dir.zip
mkdir $BASENAME-html-dir
rm $BASENAME-html-dir/figures_html/*.pdf
rm $BASENAME-html-dir/figures_html/*.png
cp $BASENAME.html ./$BASENAME-html-dir/.
cp -a figures_html ./$BASENAME-html-dir/.
zip -r $BASENAME-html-dir.zip $BASENAME-html-dir

## sweave2html from http://biostat.mc.vanderbilt.edu/wiki/Main/SweaveConvert#Converting_from_LaTeX_to_html  
