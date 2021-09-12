## Combining data. merge and dplyr:join_*
## See more details in
## https://www.rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf
## http://stat545.com/bit001_dplyr-cheatsheet.html

## I tend to use data.table now. The equivalent of left and right join are
## possible doing: e.g.,
## https://rstudio-pubs-static.s3.amazonaws.com/52230_5ae0d25125b544caab32f75f0360e775.html)
## https://stackoverflow.com/questions/12773822/why-does-xy-join-of-data-tables-not-allow-a-full-outer-join-or-a-left-join
## What I do most often
## left: merge(x, y, all.x = TRUE)
## right: merge(x, y, all.y = TRUE)
## full outer: merge(x, y, all = TRUE)




## All examples with merge from Ch. 13 of "R for dummies", 2nd ed. de
## Vries and Meys.

## state.x77 is part of datasets, so immediately available
all.states <- as.data.frame(state.x77)
all.states$Name <- rownames(state.x77) 
rownames(all.states) <- NULL


cold.states <- all.states[all.states$Frost>150, c("Name", "Frost")]
cold.states

large.states <- all.states[all.states$Area >= 100000, c("Name", "Area")]
large.states


merge(cold.states, large.states)
merge(cold.states, large.states, by = "Name")

merge(cold.states, large.states, all = TRUE)
merge(cold.states, large.states, all.x = TRUE)
merge(cold.states, large.states, all.y = TRUE)


library(dplyr)

dplyr::full_join(cold.states, large.states)
dplyr::full_join(cold.states, large.states, by = "Name")

dplyr::inner_join(cold.states, large.states)
dplyr::left_join(cold.states, large.states)
dplyr::right_join(cold.states, large.states)

###### These filter

## In first, with match in second
dplyr::semi_join(cold.states, large.states)
dplyr::semi_join(large.states, cold.states)

## In first with no match in second
dplyr::anti_join(cold.states, large.states)



## And we can use union, intersection, and setdiff

dA <- data.frame(c1 = letters[1:4], c2 = 1:4)
dB <- data.frame(c1 = letters[3:4], c2 = c(3, 14))

dplyr::union(dA, dB)
dplyr::intersect(dA, dB)

dplyr::setdiff(dA, dB)
dplyr::setdiff(dB, dA)



## A nice lookup table example, modified from Wickham's Advanced R


grades <- c(7, 8, 8, 9, 7)

info <- data.frame(
    grade = 9:7,
    desc = c("Excellent", "Good", "Poor"),
    fail = c(F, F, T)
)

## Using match
id <- match(grades, info$grade)
info[id, ]

## note this
match(grades, info$grade)
[1] 3 2 2 1 3


## Using rownames
rownames(info) <- info$grade
info[as.character(grades), ]
