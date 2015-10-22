#!/bin/bash
runKnitr-2.sh R-bioinfo-intro.Rnw purl

## ./make-html.sh R-bioinfo-intro.Rnw

## zip -r permafrost.zip permafrost

# zip -r Additional_files_R-bioinfo-intro.zip\
#  hit-table-500-text.txt AnotherDataSet.txt\
#  anage.RData lastExample.R Condition_A.txt\
#  script1.R permafrost.zip\
#  Condition_B.txt Condition_C.txt R-bioinfo-intro.R

zip -r Additional_files_R-bioinfo-intro.zip\
 hit-table-500-text.txt AnotherDataSet.txt\
 anage.RData lastExample.R Condition_A.txt\
 script1.R\
 Condition_B.txt Condition_C.txt R-bioinfo-intro.R


## runKnitr-2.sh is in the repo
