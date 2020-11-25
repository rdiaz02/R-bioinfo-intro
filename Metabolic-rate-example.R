
## PURPOSE of this:
## Use different types of plot for same data
## Illustrate split-apply-combine
## Illustrate dealing with errors and incremental building of code
## Illustrate reading of help

######
## What is in the workspace? I want it clean
rm(list = ls())

anage <-  read.table("AnAge_birds_reptiles.txt", 
                     header=TRUE, na.strings="NA")


str(anage)
head(anage)
tail(anage)
summary(anage)

## Did you notice the naming of columns?


## Simpler to do it now, so it is available later
anage$logmet <- log(anage$Metabolic.rate..W.)
anage$logbm <- log(anage$Body.mass..g)



library(car)

scatterplot(Metabolic.rate..W. ~ Body.mass..g., data = anage,
     log = "xy")

## too many lines; what are we getting rid of?
scatterplot(Metabolic.rate..W. ~ Body.mass..g., data = anage,
     log = "xy", smooth = FALSE)

## But I have birds and reptiles. What is smooth doing?
scatterplot(Metabolic.rate..W. ~ Body.mass..g. | Class,
            data = anage,
            log = "xy", smooth = FALSE)

scatterplot(Metabolic.rate..W. ~ Body.mass..g. | Class,
            data = anage,
            log = "xy", smooth = TRUE)

scatterplot(Metabolic.rate..W. ~ Body.mass..g. | Class,
            data = anage,
            log = "xy", smooth = list(smoother = loessLine,
                                      var = TRUE))

scatterplot(Metabolic.rate..W. ~ Body.mass..g. | Class,
            data = anage,
            log = "xy", smooth = list(smoother = loessLine,
                                      var = FALSE))

## Notice col
scatterplot(Metabolic.rate..W. ~ Body.mass..g. | Class,
            data = anage,
            log = "xy",
            col = c("orange", "green"))
## but nope, not everything is changeable that way: e.g., cex

scatterplot(Metabolic.rate..W. ~ Body.mass..g. | Class,
            data = anage,
            log = "xy",
            cex = c(1, 8))

scatterplot(Metabolic.rate..W. ~ Body.mass..g. | Class,
            data = anage,
            log = "xy",
            cex = c(1, 8)[Class])


scatterplot(Metabolic.rate..W. ~ Body.mass..g. | Class,
            data = anage,
            log = "xy",
            cex = c(1, 8)[anage$Class])

## does plot work?
plot(Metabolic.rate..W. ~ Body.mass..g.,
            data = anage,
            log = "xy",
            cex = c(1, 8)[anage$Class])

## let's try again. 
dx <- na.omit(anage[, c("Metabolic.rate..W.", "Body.mass..g.", "Class")])

scatterplot(Metabolic.rate..W. ~ Body.mass..g. | Class,
            data = dx,
            log = "xy",
            cex = c(1, 8))

cexo <- c(1,8)[dx$Class]

scatterplot(Metabolic.rate..W. ~ Body.mass..g. | Class,
            data = dx,
            log = "xy",
            cex = cexo) ## OK, I give up



## continue with plot

## Eh???!!!
plot(Metabolic.rate..W. ~ Body.mass..g.,
     data = anage,
     col = c("red", "blue")[anage$Class],
     log = "xy")

## What is going on?
plot(Metabolic.rate..W. ~ Body.mass..g.,
     data = anage,
     col = c("red", "blue")[factor(anage$Class)],
     log = "xy")

anage$Class <- factor(anage$Class)

## legend?
plot(Metabolic.rate..W. ~ Body.mass..g.,
     data = anage,
     col = c("salmon", "darkgreen")[Class],
     pch = c(1, 2)[Class],
     log = "xy")

legend(10, 2, legend = levels(anage$Class),
       col = c("salmon", "darkgreen"),
       pch = c(1, 2))

## adding lines more of a pain here. We will continue below. Detour for
## now to using ggplot2


## ggplot

library(ggplot2)

p1 <- ggplot(aes(x = Body.mass..g., y = Metabolic.rate..W.),
             data = anage) + geom_point()

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
p2 <- ggplot(aes(x = Body.mass..g.,
                 y = Metabolic.rate..W.,
                 color = Class),
             data = anage) +
    geom_point() 
p2

p2 + scale_x_log10() + scale_y_log10() + facet_wrap( ~ Class) +
    geom_smooth(method = "lm", se = FALSE )

## single panel
p2 + scale_x_log10() + scale_y_log10() +
    geom_smooth(method = "lm", se = FALSE )

## single panel, use other colors

p2 + scale_x_log10() + scale_y_log10() +
    geom_smooth(method = "lm", se = FALSE ) +
    scale_color_manual(values = c("red", "blue"))

library(cowplot)

## selection

birds <- anage[anage$Class == "Aves", ]

library(dplyr)
birds2 <- dplyr::filter(anage, Class == "Aves")

## creating new variables why might not always be good?
## (for log transformation)


## adding a legend interactively
plot(Metabolic.rate..W. ~ Body.mass..g.,
     data = anage,
     col = c("salmon", "darkgreen")[Class],
     pch = c(1, 2)[Class],
     log = "xy")

legend(locator(1), legend = levels(anage$Class),
       col = c("salmon", "darkgreen"),
       pch = c(1, 2))


## some more manual work

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
## where should we place the labels?

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
     ylim = c(log(0.01), max(anage$logmet)))  ## what happened?

plot(logmet ~ logbm, data = anage,
     xlab = "Log body mass (g)", ylab = "Log metabolic rate (W)",
     axes = FALSE,
     ylim = c(log(0.01), max(anage$logmet, na.rm = TRUE)))

box()
axis(side = 2, at = log(yv), labels = yv)

dev.off()

## changing par
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

nrow(anage.clean)
nrow(anage.clean2)

anage3 <- dplyr::filter(anage,
                        !is.na(Metabolic.rate..W.) &
                        !is.na(Body.mass..g.))

anage4 <- dplyr::filter(anage,
                        !(is.na(Metabolic.rate..W.) |
                          is.na(Body.mass..g.)))

summary(anage3)
summary(anage4)
identical(anage3, anage4)

## Quick and dirty version of expect_true, etc
## ALWAYS add testing code as you go along
stopifnot(identical(anage3, anage4))


## Hummm... What gives here?
identical(anage.clean, anage3)

mapply(identical, anage.clean, anage3)
## or
Map(identical, anage.clean, anage3)

## what gives?

attributes(anage.clean)
attributes(anage3)

length(attributes(anage.clean))
length(attributes(anage3))


## misleading
Map(identical, attributes(anage.clean), attributes(anage3))
names(attributes(anage3))
names(attributes(anage.clean))

for(att in names(attributes(anage3))) {
    cat("\n attribute ", att, "\n")
    print(identical(attributes(anage3)[[att]],
                    attributes(anage.clean)[[att]]))
}
## here:
## let's use lapply, just to practice

lapply(names(attributes(anage3)),
       function(u) {
           cat("\n attribute ", u, ": ", 
               identical(attributes(anage3)[[u]],
                         attributes(anage.clean)[[u]]),
               "\n")
       })

## those NULL: they are ugly!!

## sending to null (or whatever)
null <- lapply(names(attributes(anage3)),
       function(u) {
           cat("\n attribute ", u, ": ", 
               identical(attributes(anage3)[[u]],
                         attributes(anage.clean)[[u]]),
               "\n")
       })


## Notes:
##  - check Map vs mapply: where?
##  - what we did? why?
##  - arguments to plot?
##  - where is this file?

## Back to the plot

### ablines and regression
birds <- dplyr::filter(anage, Class == "Aves")

summary(lm(logmet ~ logbm, birds))

plot(logmet ~ logbm, birds)
abline(lm(logmet ~ logbm, birds))

## ok but ... we want both birds and reptiles
## let's plot both. First, we want the regression lines

mbirds <- lm(logmet ~ logbm,
             data = dplyr::filter(anage, Class == "Aves"))

## similar to
mbirds0 <- lm(logmet ~ logbm,
              data = anage, subset = (Class == "Aves"))

mrept <- lm(logmet ~ logbm,
            data = dplyr::filter(anage, Class != "Aves"))

## we will want colors
colors() ## yes, we have a few choices . How do I choose?

## Now, plot the points and then add regression lines
plot(logmet ~ logbm, anage,
     col = c("salmon", "turquoise")[Class])

abline(mbirds, col = "salmon")
abline(mrept, col = "turquoise")


## Too much work. 
## A nicer, smarter way?

## I can certainly do this. split-apply(-combine)
lapply(split(anage, anage$Class),
       function(dd) lm(logmet ~ logbm, data = dd))


plot(logmet ~ logbm, anage,
     col = c("salmon", "turquoise")[Class])

lapply(split(anage, anage$Class),
       function(dd) abline(lm(logmet ~ logbm, data = dd)))
## OK, looks doable. A few minor tweaks and we are done

############### A tangent
## What about sapply?
## Not a natural thing to do here
sapply(split(anage, anage$Class),
       function(dd) lm(logmet ~ logbm, data = dd))

sapply(split(anage, anage$Class),
       function(dd) lm(logmet ~ logbm, data = dd),
       simplify = FALSE)

## And vapply; too much of a mess here
## and not a natural application
vapply(split(anage, anage$Class),
       function(dd) lm(logmet ~ logbm, data = dd),
       FUN.VALUE = lm(logmet ~ logbm, data = anage))

## However, use vapply whenever you can instead of sapply!
## Lets continue on the tangent here. From the help of sapply

i39 <- sapply(3:9, seq) # list of vectors

sapply(i39, summary)

vapply(i39, summary,
       c(Min. = 0, "1st Qu." = 0, Median = 0, "3rd Qu." = 0, Max. = 0))

i39b <- i39
i39b[[2]] <- factor(letters[1:5])

sapply(i39b, summary)

vapply(i39, summary,
       c(Min. = 0, "1st Qu." = 0, Median = 0, "3rd Qu." = 0, Max. = 0))

## And replicate?
replicate(10, mean(rnorm(100)))

####### End tangent


colores <- c("salmon", "turquoise")

## OK, promising?
plot(logmet ~ logbm, anage, col = colores[Class])

## ;-(
lapply(split(anage, anage$Class),
       function(dd) abline(lm(logmet ~ logbm, data = dd),
                              col = colores[Class]))

## yes (and get rid of the NULLs)
junk <- lapply(split(anage, anage$Class),
       function(dd) abline(lm(logmet ~ logbm, data = dd),
                              col = colores[dd$Class]))

## What did we do? If we get side tracked
dddd <- split(anage, anage$Class)

lapply(dddd, function(u) u$Class)



## see help of "by" for another example
## of extracting coefficients by group










