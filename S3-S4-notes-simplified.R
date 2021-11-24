## Note the UseMethod. These are generics
print
plot
summary

## all methods for a generic
methods("plot")
getAnywhere(plot.TukeyHSD)


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

?methods
## print(methods(class = "lm"), byclass = TRUE)
## print(methods(class = "lm"), byclass = FALSE)



## Methods DO NOT belong to objects


## Getting the source code. From the help of methods

     ## The source code for all functions is available.  For S3 functions
     ## exported from the namespace, enter the method at the command line
     ## as ‘generic.class’.  For S3 functions not exported from the
     ## namespace, see ‘getAnywhere’ or ‘getS3method’.  For S4 methods,
     ## see ‘getMethod’.

add1.lm
getAnywhere("add1.lm")
stats:::add1.lm
getS3method("add1", "lm")
## This is exported. No problem here
kappa.lm

getAnywhere("plot.fitnessEffects")
getS3method("plot", "fitnessEffects")



########################################

####  Some practice creating classes and methods

## Let's pretend we want to deal with some very specific kinds of
## structures that have info about cholesterol, the expression of one
## gene, and the kind of experiment this was measured

## We want to convert data frames to that class, and produce some special
## plots and output when shown

library(testthat) ## for testing


## Oh, notice the signature. See "How to design programs". https://htdp.org/
##  An Edx sequence of courses: https://www.edx.org/course/how-to-code-simple-data


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


uu <- to_CG(data.frame(Cholesterol = 1:10, Gene = 11:20,
                 Kind = "Cl1"))

## Oooops!!! Ugly!!
uu
## Even uglier
summary(uu)
str(uu)
attributes(uu)

## Second attempt.
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

## go step by step here?
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
})

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

plot.Cholest_Gene <- function(x, ...) {
    class(x) <- "data.frame"
    require(ggplot2)
    ## FIXME: should I explicitly print? Hummm.. return, as orthodox?
    if (nlevels(x$Kind) >= 2 )
        p1 <- ggplot(aes(y = Cholesterol, x = Gene, col = Kind),
                     data = x) +
            facet_grid(~ Kind)
    else
        p1 <- ggplot(aes(y = Cholesterol, x = Gene), data = x)
    p1 <- p1 + geom_point()
    return(p1)
}

plot(uu)

uu2 <- uu
uu2[1:3, "Kind"] <- "Cl2"
## notice the NA, but that ain't related to our class
uu2$Kind <- as.character(uu2[, "Kind"])
uu2[1:3, "Kind"] <- "Cl2"
uu2[, "Kind"] <- factor(uu2[, "Kind"])

plot(uu2)


## Of course, this works
methods("to_CG")
methods(class = "data.frame")
methods(class = "Cholest_Gene")

## Now create summary and print for our new type of object and add tests.



## See some examples: plotFitnessLandscape
##                    POM, LOD




### A quick overview of S4


## From Wickham's Advanced R:
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

## From Wickham's Advanced R programming (section 7.5)

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

## And there are other systems, like reference classes.
