## See:
## http://www.cookbook-r.com/Manipulating_data/Converting_data_between_wide_and_long_format/
## https://rpubs.com/bradleyboehmke/data_wrangling
## Ch. 13 of "R for dummies"
## Ch. 9 of "R for data science", Wickham and Grolemund

## I focus only on dplyr::gather and dplyr::spread, which seem to be the
## preferred ways currently in the "tidyverse"

## See bottom for other approaches


## long -> wide
(df1 <- data.frame(ID  = c(letters[1:3], letters[1:3]), Y = c(1:3, 11:13),
                  Trt = c(rep("Drug", 3), rep("Plcb", 3))))

library(tidyr)

(df3 <- tidyr::spread(df1, key = Trt, value = Y))




## wide -> long

tidyr::gather(df3, key = Trt, value = Y, -ID)
tidyr::gather(df3, key = Trt, value = Y, Drug:Plcb)



## long -> wide


(df11 <- data.frame(ID  = c(letters[1:4], letters[1:4]),
                   Sex = rep(c("M", "F"), 4),
                   Y = c(1:4, 11:14),
                   Trt = c(rep("Drug", 4), rep("Plcb", 4))))


(df13 <- tidyr::spread(df11, key = Trt, value = Y))

## to long format
tidyr::gather(df13, key = Trt, value = A_Y, Drug:Plcb)

tidyr::gather(df13, key = Trt, value = A_Y, -(ID:Sex))





####################################################
############                            ############
############       Other approaches     ############
############                            ############   
####################################################


### long -> wide

(df2 <- unstack(x = df1, form = Y ~ Trt))
## df2 is wide, but we lost the IDs. Bad.

## probably heavily oriented towards time-related data.
(df2 <- reshape(data = df1, direction = "wide",
                idvar = "ID", v.names = "Y",
                timevar = "Trt"))

library(reshape2)
reshape2::dcast(data = df1, ID ~ Trt, value.var = "Y")


(reshape2::dcast(data = df11, ID + Sex ~ Trt, value.var = "Y"))



### wide -> long
stack(x = df3)
reshape2::melt(df3, id.vars = "ID")



