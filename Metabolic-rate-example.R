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
     log = "xy", spread = FALSE)
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

p1 + scale_x_log10() + scale_y_log10()

p1 + scale_x_log10() + scale_y_log10() + facet_wrap( ~ Class)

p1 + scale_x_log10() + scale_y_log10() + facet_wrap( ~ Class) +
    geom_smooth(method = "lm")

p1 + scale_x_log10() + scale_y_log10() + facet_wrap( ~ Class) +
    geom_smooth(method = "lm", se = FALSE )


p2 <- ggplot(aes(x = Body.mass..g., y = Metabolic.rate..W., color = Class),
             data = anage) +
    geom_point() 
p2

p2 + scale_x_log10() + scale_y_log10() + facet_wrap( ~ Class) +
    geom_smooth(method = "lm", se = FALSE )

## library(cowplot)
