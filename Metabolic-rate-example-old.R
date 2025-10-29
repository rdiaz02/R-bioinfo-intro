## For me: recall to  (setq ess-eval-visibly-p t)

### PURPOSE
## PURPOSE of this:
## Use different types of plot for same data
## Illustrate split-apply-combine
## Illustrate dealing with errors and incremental building of code
## Illustrate reading of help

### Clean workspace and load data
## What is in the workspace? I want it clean
rm(list = ls())

anage <-  read.table("AnAge_birds_reptiles.txt",
                     header = TRUE, na.strings = "NA")


str(anage)
head(anage)
tail(anage)
summary(anage)

## Simpler to do it now, so it is available later
anage$logmet <- log(anage$Metabolic.rate..W.)
anage$logbm <- log(anage$Body.mass..g)


## Humm??
plot(Metabolic.rate..W. ~ Body.mass..g.,
     data = anage,
     col = c("red", "blue")[anage$Class],
     log = "xy")

library(car) ## We might want to move it up in the file
## Hummm... does not work
scatterplot(Metabolic.rate..W. ~
                Body.mass..g. | Class,
            data = anage,
            log = "xy",
            cex = c(1, 8)[anage$Class])



## This used not to be necessary
anage$Class <- factor(anage$Class)

plot(Metabolic.rate..W. ~ Body.mass..g.,
     data = anage,
     col = c("red", "blue")[anage$Class],
     log = "xy")



### skip

## We did this already, so skip
## Did you notice the naming of columns?

library(car)

scatterplot(Metabolic.rate..W. ~ Body.mass..g.,
            data = anage,
            log = "xy")

## too many lines; what are we getting rid of?
scatterplot(Metabolic.rate..W. ~ Body.mass..g.,
            data = anage,
            log = "xy",
            smooth = FALSE)

## But I have birds and reptiles. What is smooth doing?
scatterplot(Metabolic.rate..W. ~
                Body.mass..g. | Class,
            data = anage,
            log = "xy",
            smooth = FALSE)

scatterplot(Metabolic.rate..W. ~
                Body.mass..g. | Class,
            data = anage,
            log = "xy", smooth = TRUE)

## smooth each and add variance
scatterplot(Metabolic.rate..W. ~
                Body.mass..g. | Class,
            data = anage,
            log = "xy",
            smooth = list(smoother = loessLine,
                          var = TRUE))

## smooth explicitly, and remove variance envelope
scatterplot(Metabolic.rate..W. ~
                Body.mass..g. | Class,
            data = anage,
            log = "xy",
            smooth = list(smoother = loessLine
                        ## , var = FALSE ## uncomment if you don't want var.
                        ## , var = TRUE  ## uncomment if you want variance.
                          ))

## Notice colors. Working as expected
scatterplot(Metabolic.rate..W. ~
                Body.mass..g. | Class,
            data = anage,
            log = "xy",
            col = c("orange", "green"))

## But nope, not everything is changeable that way: e.g., cex
scatterplot(Metabolic.rate..W. ~
                Body.mass..g. | Class,
            data = anage,
            log = "xy",
            cex = c(1, 8))

## Oooops
scatterplot(Metabolic.rate..W. ~
                Body.mass..g. | Class,
            data = anage,
            log = "xy",
            cex = c(1, 8)[Class])



## This is no longer relevant as it works
## What is happening here?? (For details of stringsAsFactors: https://developer.r-project.org/Blog/public/2020/02/16/stringsasfactors/index.html)

## Yes, it is a factor
anage$Class


## Now
plot(Metabolic.rate..W. ~ Body.mass..g.,
     data = anage,
     log = "xy",
     cex = c(1, 3)[anage$Class])

## Does not do what we want
scatterplot(Metabolic.rate..W. ~
                Body.mass..g. | Class,
            data = anage,
            log = "xy",
            cex = c(1, 3)[anage$Class])



## let's try again.
dx <- na.omit(anage[, c("Metabolic.rate..W.",
                        "Body.mass..g.",
                        "Class")])

scatterplot(Metabolic.rate..W. ~
                Body.mass..g. | Class,
            data = dx,
            log = "xy",
            cex = c(1, 3))

cexo <- c(1,3)[dx$Class]

scatterplot(Metabolic.rate..W. ~
                Body.mass..g. | Class,
            data = dx,
            log = "xy",
            cex = cexo) ## OK, I give up


## continue with plot

######################################################################
## /skip

## ## This would break if Class is not a factor
plot(Metabolic.rate..W. ~ Body.mass..g.,
     data = anage,
     col = c("red", "blue")[anage$Class],
     log = "xy")


## legend?
plot(Metabolic.rate..W. ~ Body.mass..g.,
     data = anage,
     col = c("salmon", "darkgreen")[Class],
     pch = c(1, 2)[Class],
     log = "xy")

legend(10, 2, legend = levels(anage$Class),
       col = c("salmon", "darkgreen"),
       pch = c(1, 2))



## Remember: if I want to produce a pdf for real (say, a paper)
## change settings as needed and surround by pdf() and dev.off()

## Also, notice labels of y axis
op <- par(las = 1)

plot(Metabolic.rate..W. ~ Body.mass..g.,
     data = anage,
     col = c("salmon", "darkgreen")[Class],
     pch = c(1, 2)[Class],
     log = "xy")

legend(10, 2, legend = levels(anage$Class),
       col = c("salmon", "darkgreen"),
       pch = c(1, 2))

par(op)


## adding lines more of a pain here. We will continue below. Detour for
## now to using ggplot2

###############################################################
## /skip


## ggplot

library(ggplot2)

p1 <- ggplot(aes(x = Body.mass..g.,
                 y = Metabolic.rate..W.),
             data = anage) + geom_point()


p1

## how can I use log?

p1 + scale_x_log10() + scale_y_log10()

## conditioning (using facet_wrap; there is also facet_grid)
p1 + scale_x_log10() + scale_y_log10() + facet_wrap( ~ Class)

p1 + scale_x_log10() + scale_y_log10() +
    facet_wrap( ~ Class) +
    geom_smooth(method = "lm")

p1 + scale_x_log10() + scale_y_log10() +
    facet_wrap( ~ Class) +
    geom_smooth(method = "lm", se = FALSE )

## Using color per class. Not using facet_wrap
p2 <- ggplot(aes(x = Body.mass..g.,
                 y = Metabolic.rate..W.,
                 color = Class),
             data = anage) +
    geom_point()
p2

p2 + scale_x_log10() + scale_y_log10() +
    facet_wrap( ~ Class) +
    geom_smooth(method = "lm", se = FALSE )

## single panel
p2 + scale_x_log10() + scale_y_log10() +
    geom_smooth(method = "lm", se = FALSE )

## single panel, use other colors (which are much worse!)

p2 + scale_x_log10() + scale_y_log10() +
    geom_smooth(method = "lm", se = FALSE ) +
    scale_color_manual(values = c("red", "blue"))

library(cowplot)

## selection

birds <- anage[anage$Class == "Aves", ]

library(dplyr)
birds2 <- dplyr::filter(anage, Class == "Aves")



## adding a legend interactively
plot(Metabolic.rate..W. ~ Body.mass..g.,
     data = anage,
     col = c("salmon", "darkgreen")[Class],
     pch = c(1, 2)[Class],
     log = "xy")

## Emacs: does not work with xwidget
legend(locator(1), legend = levels(anage$Class),
       col = c("salmon", "darkgreen"),
       pch = c(1, 2))


## some more manual work

## todo
## - labels with at to use custom tick marks
## complete cases and table without NAs
## abline for plot for reptiles and aves

plot(logmet ~ logbm, data = anage,
     xlab = "Body mass (g)",
     ylab = " Metabolic rate (W)",
     axes = FALSE)

box()

## help for labels is not very helpful?
## where should we place the labels?

summary(anage)
## range of metabolic rate
exp(seq(from = -4.3, to = 2, length.out = 6))

yv <- c(0.01, 0.05, 0.15, 0.6, 2, 7)
log(yv)

axis(side = 2, at = log(yv), labels = yv)

## Hummm, ugly for several reasons

plot(logmet ~ logbm, data = anage,
     xlab = "Body mass (g)",
     ylab = "Metabolic rate (W)",
     axes = FALSE,
     ylim = c(log(0.01),
              max(anage$logmet)))  ## what happened?

plot(logmet ~ logbm, data = anage,
     xlab = "body mass (g)",
     ylab = " metabolic rate (W)",
     axes = FALSE,
     ylim = c(log(0.01),
              max(anage$logmet, na.rm = TRUE)))

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
    complete.cases(
        anage[, c("Class",
                  "Metabolic.rate..W.",
                  "Body.mass..g.")])
  , ]
summary(anage.clean)


anage.clean2 <- na.omit(
    anage[, c("Class", "Metabolic.rate..W.", "Body.mass..g.")])
summary(anage.clean2)

nrow(anage.clean)
nrow(anage.clean2)

stopifnot(nrow(anage.clean) == nrow(anage.clean2))

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


all(mapply(identical, anage.clean, anage3))


## what gives?

attributes(anage.clean)
attributes(anage3)

length(attributes(anage.clean))
length(attributes(anage3))


## Possibly misleading. Not anymore
Map(identical, attributes(anage.clean), attributes(anage3))

names(attributes(anage3))
names(attributes(anage.clean))

stopifnot(all(names(attributes(anage.clean)) == names(attributes(anage3))))

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
              data = anage,
              subset = (Class == "Aves"))

stopifnot(
    identical(
        coefficients(mbirds0), coefficients(mbirds)))

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
       function(dd)
           abline(lm(logmet ~ logbm, data = dd)))
## OK, looks doable. A few minor tweaks and we are done

############### A tangent
## What about sapply?
## Not a natural thing to do here ?
sapply(split(anage, anage$Class),
       function(dd) lm(logmet ~ logbm, data = dd))

sapply(split(anage, anage$Class),
       function(dd) lm(logmet ~ logbm, data = dd),
       simplify = FALSE)
plot(logmet ~ logbm, anage,
     col = c("salmon", "turquoise")[Class])

sapply(split(anage, anage$Class),
       function(dd) abline(lm(logmet ~ logbm, data = dd)))

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
       c(Min. = 0, "1st Qu." = 0, Median = 0,
         mean = 0, "3rd Qu." = 0, Max. = 0))

i39b <- i39
i39b[[2]] <- factor(letters[1:5])

sapply(i39b, summary)

vapply(i39b, summary,
       c(Min. = 0, "1st Qu." = 0, Median = 0,
         mean = 0, "3rd Qu." = 0, Max. = 0))

vapply(i39b, summary,
       rep(0, 6))


## And replicate?
replicate(1000, mean(rnorm(100)))

####### End tangent


colores <- c("salmon", "turquoise")

## OK, promising?
plot(logmet ~ logbm, anage, col = colores[Class])

## ;-(
lapply(split(anage, anage$Class),
       function(dd)
           abline(lm(logmet ~ logbm, data = dd),
                  col = colores[Class]))

## yes (and get rid of the NULLs)

junk <- lapply(
    split(anage, anage$Class),
    function(dd)
        abline(lm(logmet ~ logbm,
                  data = dd),
               col = colores[dd$Class]))


## What did we do? If we get side tracked
dddd <- split(anage, anage$Class)

lapply(dddd, function(u) u$Class)



## see help of "by" for another example
## of extracting coefficients by group
