#!/bin/bash
runKnitr.sh R-bioinfo-intro.Rnw
./make-html.sh R-bioinfo-intro.Rnw
zip -r Additional_files_R-bioinfo-intro.zip\
 hit-table-500-text.txt AnotherDataSet.txt\
 anage.RData lastExample.R Condition_A.txt\
 Condition_B.txt Condition_C.txt R-bioinfo-intro.R
