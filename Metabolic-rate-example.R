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

#### Aqui nos quedamos

## selection

birds <- anage[anage$Class == "Aves", ]

library(dplyr)
birds2 <- dplyr::filter(anage, Class == "Aves")

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



plot(logmet ~ logbm, data = anage,
     xlab = "Log body mass (g)",
     ylab = "Log metabolic rate (W)",
     axes = FALSE)

box()

## help for labels is not very helpful?
## where?

summary(anage)
exp(seq(from = -4.3, to = 2, length.out = 6))

yv <- c(0.01, 0.05, 0.15, 0.6, 2, 7) 
log(yv)

axis(side = 2, at = log(yv), labels = yv)

## Hummm, ugly for several reasons

par(las = 1)

plot(logmet ~ logbm, data = anage,
     xlab = "Log body mass (g)", ylab = "Log metabolic rate (W)",
     axes = FALSE,
     ylim = c(log(0.01), max(anage$logmet, na.rm = TRUE)))

plot(logmet ~ logbm, data = anage,
     xlab = "Log body mass (g)", ylab = "Log metabolic rate (W)",
     axes = FALSE,
     ylim = c(log(0.01), max(anage$logmet)))


box()
axis(side = 2, at = log(yv), labels = yv)

dev.off()

op <- par(las = 1)

plot(logmet ~ logbm, data = anage,
     xlab = "Log body mass (g)", ylab = "Log metabolic rate (W)",
     axes = FALSE, ylim = c(log(0.01), max(anage$logmet, na.rm = TRUE)))
box()
axis(side = 2, at = log(yv), labels = yv)

par(op)


anage.clean <- anage[
    complete.cases(anage[, c("Class",
                             "Metabolic.rate..W.",
                             "Body.mass..g.")])
  , ]
summary(anage.clean)

anage.clean2 <- na.omit(
    anage[, c("Class", "Metabolic.rate..W.", "Body.mass..g.")])
summary(anage.clean2)

anage3 <- dplyr::filter(anage, !is.na(Metabolic.rate..W.) & !is.na(Body.mass..g.))
anage4 <- dplyr::filter(anage, !(is.na(Metabolic.rate..W.) | is.na(Body.mass..g.)))
summary(anage3)
summary(anage4)
identical(anage3, anage4)

## Hummm...
identical(anage.clean, anage3)

mapply(identical, anage.clean, anage3)
## what gives?

attributes(anage.clean)
attributes(anage3)

## misleading
mapply(identical, attributes(anage.clean), attributes(anage3))
names(attributes(anage3))
names(attributes(anage.clean))

for(att in names(attributes(anage3))) {
    cat("\n attribute ", att, "\n")
    print(identical(attributes(anage3)[[att]], attributes(anage.clean)[[att]]))
}


### ablines and regression

birds <- dplyr::filter(anage, Class == "Aves")

summary(lm(logmet ~ logbm, birds))

plot(logmet ~ logbm, birds)
abline(lm(logmet ~ logbm, birds))

## ok but ...

mbirds <- lm(logmet ~ logbm, data = dplyr::filter(anage, Class == "Aves"))
## similar to
mbirds0 <- lm(logmet ~ logbm, data = anage, subset = (Class == "Aves"))

mrept <- lm(logmet ~ logbm, data = dplyr::filter(anage, Class != "Aves"))

colors()

plot(logmet ~ logbm, anage, col = c("salmon", "turquoise")[Class])
abline(mbirds, col = "salmon")
abline(mrept, col = "turquoise")



## nicer, smarter?

lapply(split(anage, anage$Class),
       function(dd) lm(logmet ~ logbm, data = dd))

plot(logmet ~ logbm, anage, col = c("salmon", "turquoise")[Class])
lapply(split(anage, anage$Class),
       function(dd) abline(lm(logmet ~ logbm, data = dd)))



plot(logmet ~ logbm, anage, col = c("salmon", "turquoise")[Class])
lapply(split(anage, anage$Class),
       function(dd) abline(lm(logmet ~ logbm, data = dd)))


colores <- c("salmon", "turquoise")

## hummm...
plot(logmet ~ logbm, anage, col = colores[Class])
lapply(split(anage, anage$Class),
       function(dd) abline(lm(logmet ~ logbm, data = dd,
                              col = colores[Class])))

lapply(split(anage, anage$Class),
       function(dd) abline(lm(logmet ~ logbm, data = dd),
                              col = colores[dd$Class]))

## if we get side tracked
dddd <- split(anage, anage$Class)

lapply(dddd, function(u) u$Class)
lapply(split(anage, anage$Class),
       function(dd) abline(lm(logmet ~ logbm, data = dd,
                              col = colores[Class])))


