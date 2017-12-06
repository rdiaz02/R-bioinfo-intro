## See:
## http://www.cookbook-r.com/Manipulating_data/Converting_data_between_wide_and_long_format/
## https://rpubs.com/bradleyboehmke/data_wrangling
## Ch. 13 of "R for dummies"

df1 <- data.frame(ID  = c(letters[1:3], letters[1:3]), Y = c(1:3, 11:13),
                  Trt = c(rep("Drug", 3), rep("Plcb", 3)))

(df2 <- unstack(x = df1, form = Y ~ Trt))

## df2 is wide, but we lost the IDs. Bad.


## probably heavily oriented towards time-related data.
(df2 <- reshape(data = df1, direction = "wide",
                idvar = "ID", v.names = "Y",
                timevar = "Trt"))


library(tidyr)

(df3 <- tidyr::spread(df1, key = Trt, value = Y))

library(reshape2)
reshape2::dcast(data = df1, ID ~ Trt, value.var = "Y")



## to long format
stack(x = df3)

tidyr::gather(df3, key = Trt, value = Y, -ID)
reshape2::melt(df3, id.vars = "ID")



## Probably recommended way is to use gather and spread from tidyr.


df11 <- data.frame(ID  = c(letters[1:4], letters[1:4]),
                   Sex = rep(c("M", "F"), 4),
                   Y = c(1:4, 11:14),
                   Trt = c(rep("Drug", 4), rep("Plcb", 4)))

## to wide
(df13 <- tidyr::spread(df11, key = Trt, value = Y))
(reshape2::dcast(data = df11, ID + Sex ~ Trt, value.var = "Y"))

## to long
tidyr::gather(df13, )




