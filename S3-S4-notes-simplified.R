## Examples of S3 and S4 OOP in R as kind of quick notes.



## Note the UseMethod. These are generics
print
plot
summary

## all methods for a generic (among those attached in search path)
methods("plot")
getAnywhere(plot.TukeyHSD) ## this is not exported: see the "*"

## Compare this
plot.ts
stats::plot.ts
## The first two fail, the third succeeds
plot.TukeyHSD
stats::plot.TukeyHSD ## Note what it says
stats:::plot.TukeyHSD

## What is available changes depending on what packages we have loaded.
library(OncoSimulR)
methods("plot")
## lots more now. Why? For now, see plot.fitnessEffects
## Oh, and notice the "*" (not exported)

## or plot.genotype_fitness_matrix
getAnywhere(plot.fitnessEffects)
plot.fitnessEffects
OncoSimulR::plot.fitnessEffects
OncoSimulR:::plot.fitnessEffects

## all generics for a class
methods(class = "lm")
methods(class = "fitnessEffects")
methods(class = "genotype_fitness_matrix")


## Again, lm. Two ways of displaying
methods(class = "lm")
## Show with the class
print(methods(class = "lm"), byclass = FALSE)
## See ?methods for details

## Methods DO NOT belong to objects

## We've seen this above. To repeat:
## Getting the source code. From the help of methods

     ## The source code for all functions is available.  For S3 functions
     ## exported from the namespace, enter the method at the command line
     ## as ‘generic.class’.  For S3 functions not exported from the
     ## namespace, see ‘getAnywhere’ or ‘getS3method’.  For S4 methods,
     ## see ‘getMethod’.

## Let us try that
##   we know add1.lm exists, from the above call to methods(class = "lm")
add1.lm
getAnywhere("add1.lm")
stats:::add1.lm
getS3method("add1", "lm")
## This is exported. No problem here
kappa.lm
## What about packages
getAnywhere("plot.fitnessEffects")
getS3method("plot", "fitnessEffects")



########################################

####  Some practice creating classes and methods

## Let's pretend we want to deal with some very specific kinds of
## structures that have info about cholesterol, the expression of one
## gene, and the kind of experiment this was measured.

## We want to convert data frames to that class, and produce some special
## plots and output when shown

## Testing. We must test our code. Several approaches, I'll use testthat
library(testthat) ## for testing

## Oh, notice the signature. See "How to design programs". https://htdp.org/
##  An Edx sequence of courses: https://www.edx.org/course/how-to-code-simple-data


## We start writing the generic.
## plot, print, summary, etc, are generics
## and we now want a generic that will take different types of objects
## and turn them into our new kinds of objects.
## For example from data.frame to our object or from matrix to our object
## Then, we will write to_CG.data.frame, etc. This will be
## the implementation for the specific class (data.frame), the method.


## object -> Cholest_Gene object
## General converter to Cholest_Gene object.
to_CG <- function(x, ...) {
  UseMethod("to_CG")
}


## data.frame -> Cholest_Gene object
## Take a data frame and return (if possible) a Cholest_Gene object.
to_CG.data.frame <- function(x) {
    cns <- c("Cholesterol", "Gene", "Kind")
    if (!(all(colnames(x) %in% cns)))
        stop(paste("Column names are not ", cns))
    tmp <- x[, cns]
    ## Notice I do not set this to be of data.frame class
    class(tmp) <- c("Cholest_Gene")
    return(tmp)
}


## Try it
uu <- to_CG(data.frame(Cholesterol = 1:10, Gene = 11:20,
                 Kind = "Cl1"))

## Oooops!!! Ugly!!
uu
## Even uglier
summary(uu)
str(uu)
attributes(uu)

## Second attempt.

## data.frame -> Cholest_Gene object
## Take a data frame and return (if possible) a Cholest_Gene object.
to_CG.data.frame <- function(x) {
    cns <- c("Cholesterol", "Gene", "Kind")
    if (!(all(colnames(x) %in% cns)))
        stop(paste("Column names are not ", cns))
    tmp <- x[, cns]
    tmp$Kind <- factor(tmp$Kind)
    ## Do you know why do I preserve the data.frame class? ...
    class(tmp) <- c("Cholest_Gene", class(tmp)) ## "data.frame")
    return(tmp)
}

uu <- to_CG(data.frame(Cholesterol = 1:10, Gene = 11:20,
                 Kind = "Cl1"))

## Using existing functionality for free
summary(uu)
print(uu)
uu

## Let's do something more sophisticated

## Cholest_Gene object -> printed Cholest_Gene object
## Print a Cholest_Gene object.
print.Cholest_Gene <- function(x) {
    u <- x[, c(1, 2)]
    class(u) <- "data.frame"
    print(u)
    cat("\n hola; now printing summary of first column \n")
    print(summary(x[, 1]))
}


uu
## Of course, this still works
summary(uu)
## Notice what is says about class
str(uu)
attributes(uu)

to_CG(cbind(Cholesterol = 1:10, Gene = 11:20))
## But that was ugly? write a default that will fail gracefully
## And we can extend as needs arise.

## arbitrary object -> failure message if no method
## Return error message if there is no specific method
## to convert from that class to Cholest_Gene class
to_CG.default <- function(x) {
    stop("For now, only methods for data.frame are available.")
}

## Much better
to_CG(cbind(Cholesterol = 1:10, Gene = 11:20))


## Now, test!!!!!!!!!!!! Really, do.
## at least:
##  - that we can create a legitimate one from a data.frame
##  - that it fails as we expect when data.frame with missing columns
##  - that it fails as we expect when not a data.frame

library(testthat)

## go step by step here? this has too many newlines, just
## to improve readability
## What we will do:
##   Run the block (and see it fails)
##   Run each internal call (to debug)

## Of course, we first had to write this code

test_that("minimal conversions and failures", {

    expect_s3_class(to_CG(data.frame(Cholesterol = 1:10, Gene = 11:20,
                                     Kind = "Cl1")), "Cholest_Gene")


    expect_error(to_CG(cbind(Cholesterol = 1:10, Gene = 11:20)),
                 "For now, only methods for data.frame are available",
                 fixed = TRUE)

    expect_error(to_CG(data.frame(Cholesterol = 1:10, Geni = 11:20,
                                     Kind = "Cl1")),
                 "Column names are not ",
                 fixed = TRUE)

    expect_s3_class(to_CG(data.frame(Cholesterol = 1:10, Gene = 11:20,
                                     Kind = "Cl1",
                                     whatever = "abcd")),
                    "Cholest_Gene")

})
## NOS QUEDAMOS AQUI! FIXME
## eh? This fails! And the output is ugly!!
expect_s3_class(to_CG(data.frame(Cholesterol = 1:10, Gene = 11:20,
                                 Kind = "Cl1",
                                 whatever = "abcd")),
                "Cholest_Gene")
## why??

dummy <- to_CG(data.frame(Cholesterol = 1:10, Gene = 11:20,
                                 Kind = "Cl1",
                                 whatever = "abcd"))
## OK, it failed. But why?
debugonce(to_CG.data.frame)

dummy <- to_CG(data.frame(Cholesterol = 1:10, Gene = 11:20,
                                 Kind = "Cl1",
                                 whatever = "abcd"))

## Fix the code: we reverted the %in%

to_CG.data.frame <- function(x) {
    cns <- c("Cholesterol", "Gene", "Kind")
    if (!(all(cns %in% colnames(x))))
        stop(paste("Column names are not ",
                   paste(cns, collapse = " ")))
    tmp <- x[, cns]
    tmp$Kind <- factor(tmp$Kind)
    class(tmp) <- c("Cholest_Gene", class(x))
    return(tmp)
}

## Add the test the failed to the set of tests

test_that("minimal conversions and failures", {
    expect_s3_class(to_CG(data.frame(Cholesterol = 1:10,
                                     Gene = 11:20,
                                     Kind = "Cl1")),
                    "Cholest_Gene")
    expect_error(to_CG(cbind(Cholesterol = 1:10, Gene = 11:20)),
                 "For now, only methods for data.frame are available",
                 fixed = TRUE)
    expect_error(to_CG(data.frame(Cholesterol = 1:10, Geni = 11:20,
                                  Kind = "Cl1")),
                 "Column names are not",
                 fixed = TRUE)
    expect_s3_class(to_CG(data.frame(Cholesterol = 1:10, Gene = 11:20,
                                     Kind = "Cl1",
                                     whatever = "abcd")), "Cholest_Gene")
})

## play with some plotting code

## Cholest_Gene object -> ggplot object
## Produce a ggplot of a Cholest_Gene object.
plot.Cholest_Gene <- function(x, ...) {
  class(x) <- "data.frame"
  require(ggplot2)
  ## FIXME: should I explicitly print? Hummm.. return, as orthodox?
  if (nlevels(x$Kind) >= 2)
    p1 <- ggplot(aes(y = Cholesterol, x = Gene, col = Kind),
                 data = x) +
      facet_grid(~ Kind)
  else
    p1 <- ggplot(aes(y = Cholesterol, x = Gene), data = x)
  p1 <- p1 + geom_point()
  return(p1)
}

plot(uu)


## skip : Now, add levels to Kind. But note what happens
uu2 <- uu
uu2[1:3, "Kind"] <- "Cl2"
## notice the NA, but that ain't related to our class
uu2$Kind <- as.character(uu2[, "Kind"])
uu2[1:3, "Kind"] <- "Cl2"
uu2[, "Kind"] <- factor(uu2[, "Kind"])

plot(uu2)
## </skip

## Of course, this works
methods("to_CG")
methods(class = "data.frame")
methods(class = "Cholest_Gene")

## Now create summary and print for our new type of object and add tests.



## See some examples: plotFitnessLandscape
##                    POM, LOD




### A quick overview of S4


## From Wickham's Advanced R (1st ed)
## S4 works in a similar way to S3, but it adds formality and
## rigour. Methods still belong to functions, not classes, but:

##     Classes have formal definitions which describe their fields and
##     inheritance structures (parent classes).

##     Method dispatch can be based on multiple arguments to a generic
##     function, not just one.

##     There is a special operator, @, for extracting slots (aka fields)
##     from an S4 object.



library(Matrix)
showMethods(class = "Matrix")
## From PatrickBurns: http://www.burns-stat.com/r-navigation-tools/
##  S3 generics can mutate to be both S3 and S4 generic:

## Compare these two between a session of R without loading Matrix
showMethods("print")
print


## Access to elements
m1 <- Matrix(1:9, nrow = 3)
m2 <- Diagonal(5)

x <- 0:10
y <- c(26, 17, 13, 12, 20, 5, 9, 8, 5, 4, 8)
fit1 <- lm(y ~ x)

class(fit1)
is.list(fit1)
isS4(fit1)
print(fit1)
stats:::print.lm(fit1)
fit1
names(fit1)
fit1$coefficients
## don't do that for real. Use coefficients
coefficients(fit1)
isS4(m1)
is.list(m1)
class(m1)
slotNames(m1)
slotNames(m2)
m1
m1@Dim
m1@x
m2@Dim

## From Wickham's Advanced R programming (1st ed., section 7.5)

## Three OO systems is a lot for one language, but for most R programming,
## S3 suffices. In R you usually create fairly simple objects and methods
## for pre-existing generic functions like print() , summary() , and
## plot() . S3 is well suited to this task, and the majority of OO code
## that I have written in R is S3. S3 is a little quirky, but it gets the
## job done with a minimum of code.

## If you are creating more complicated systems of interrelated objects,
## S4 may be more appropriate. (...) S4 is also used extensively by
## Bioconductor packages, which need to model complicated
## interrelationships between biological objects.


## From Wickham's Advanced R programming (2nd ed., section 16.1)

## Overall, when picking an OO system, I recommend that you default to S3. S3
## is simple, and widely used throughout base R and CRAN. While it’s far from
## perfect, its idiosyncrasies are well understood and there are known
## approaches to overcome most shortcomings. If you have an existing
## background in programming you are likely to lean towards R6, because it
## will feel familiar. I think you should resist this tendency for two
## reasons. Firstly, if you use R6 it’s very easy to create a non-idiomatic
## API that will feel very odd to native R users, and will have surprising
## pain points because of the reference semantics. Secondly, if you stick to
## R6, you’ll lose out on learning a new way of thinking about OOP that gives
## you a new set of tools for solving problems.
