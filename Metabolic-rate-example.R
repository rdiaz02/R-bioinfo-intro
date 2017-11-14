rm(list = ls())
anage <-  read.table("AnAge_birds_reptiles.txt", 
                     header=TRUE, na.strings="NA", 
                     strip.white=TRUE)


## What is in the workspace? I want it clean
## Not needed from above rm
## ls()

str(anage)
head(anage)
tail(anage)
summary(anage)
## Did you notice the naming of columns?



plot(Metabolic.rate..W. ~ Body.mass..g., data = anage)

plot(Metabolic.rate..W. ~ Body.mass..g., data = anage,
     log = "xy")

anage$logmet <- log(anage$Metabolic.rate..W.)
anage$logbm <- log(anage$Body.mass..g)

plot(logmet ~ logbm, data = anage)

library(car)


scatterplot(Metabolic.rate..W. ~ Body.mass..g., data = anage,
     log = "xy")

## dotted lines annoying
scatterplot(Metabolic.rate..W. ~ Body.mass..g., data = anage,
     log = "xy", spread = FALSE)

## but I have birds and reptiles
scatterplot(Metabolic.rate..W. ~ Body.mass..g. | Class,
            data = anage,
            log = "xy", spread = FALSE)

## with plot?
plot(Metabolic.rate..W. ~ Body.mass..g.,
     data = anage,
     col = c("red", "blue")[Class],
     log = "xy")
## did you see the warnings?

## legend?
plot(Metabolic.rate..W. ~ Body.mass..g.,
     data = anage,
     col = c("salmon", "darkgreen")[Class],
     pch = c(1, 2)[Class],
     log = "xy")

legend(10, 2, legend = levels(anage$Class),
       col = c("salmon", "darkgreen"),
       pch = c(1, 2))

## adding lines more of a pain here.


## ggplot

library(ggplot2)

p1 <- ggplot(aes(x = Body.mass..g., y = Metabolic.rate..W.), data = anage) +
    geom_point()

p1

## how can I use log?

p1 + scale_x_log10() + scale_y_log10()

## conditioning
p1 + scale_x_log10() + scale_y_log10() + facet_wrap( ~ Class)

p1 + scale_x_log10() + scale_y_log10() + facet_wrap( ~ Class) +
    geom_smooth(method = "lm")

p1 + scale_x_log10() + scale_y_log10() + facet_wrap( ~ Class) +
    geom_smooth(method = "lm", se = FALSE )

## using color per class
p2 <- ggplot(aes(x = Body.mass..g., y = Metabolic.rate..W., color = Class),
             data = anage) +
    geom_point() 
p2

p2 + scale_x_log10() + scale_y_log10() + facet_wrap( ~ Class) +
    geom_smooth(method = "lm", se = FALSE )

## single panel
p2 + scale_x_log10() + scale_y_log10() +
    geom_smooth(method = "lm", se = FALSE )

## nope, does not work if using colors, etc

p2 + scale_x_log10() + scale_y_log10() +
    geom_smooth(method = "lm", se = FALSE ) +
    scale_color_manual(values = c("red", "blue"))



library(cowplot)


## selection

birds <- anage[anage$Class == "Aves", ]

library(dplyr)
birds2 <- dplyr::filter(anage, Class == "Aves", )

## creating new variables and why might not always be good?
## (for log transformation)




plot(Metabolic.rate..W. ~ Body.mass..g.,
     data = anage,
     col = c("salmon", "darkgreen")[Class],
     pch = c(1, 2)[Class],
     log = "xy")

legend(locator(1), legend = levels(anage$Class),
       col = c("salmon", "darkgreen"),
       pch = c(1, 2))


## todo
## - labels con at para etiquetar tick marks
## complete cases y table sin NAs
## abline para plot para reptiles y aves
