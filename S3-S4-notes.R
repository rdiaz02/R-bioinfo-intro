
## Note the UseMethod. These are generics
print
plot
summary

## all methods for a generic
methods("plot")
## compare to: 
apropos("^plot\\..*")
find("^plot\\..*")
find("^plot\\..*", simple.words = FALSE)

library(OncoSimulR)
## lots more now. Why? For now, see plot.fitnessEffects
## or plot.genotype_fitness_matrix
methods("plot")
apropos("^plot\\..*")
find("^plot\\..*", simple.words = FALSE)



## all generics for a class
methods(class = "genotype_fitness_matrix")
methods(class = "lm")

print(methods(class = "lm"), byclass = TRUE)
print(methods(class = "lm"), byclass = FALSE)

## methods DO NOT belong to objects


## getting the source code. From the help of methods

     ## The source code for all functions is available.  For S3 functions
     ## exported from the namespace, enter the method at the command line
     ## as ‘generic.class’.  For S3 functions not exported from the
     ## namespace, see ‘getAnywhere’ or ‘getS3method’.  For S4 methods,
     ## see ‘getMethod’.

add1.lm
getAnywhere("add1.lm")
stats:::add1.lm

kappa.lm



## create new types of objects

library(testthat)

## object -> Cholest_Gene object
## Generic converter to Cholest_Gene object.
to_CG <- function(x, ...) {
    UseMethod("to_CG")
}


## data.frame -> Cholest_Gene object
## Take a data frame and return (if possible) a Cholest_Gene object.
to_CG.data.frame <- function(x) {
    cns <- c("Cholesterol", "Gene", "Class")
    if (!(all(colnames(x) %in% cns)))
        stop(paste("Column names are not ", cns))
    tmp <- x[, cns]
    ## No data.frame class
    class(tmp) <- c("Cholest_Gene")
    return(tmp)
}


uu <- to_CG(data.frame(Cholesterol = 1:10, Gene = 11:20,
                 Class = "Cl1"))

uu
summary(uu)
str(uu)
attributes(uu)

## try it without preserving the data.frame class, and then print or
## summarize and object of that kind


to_CG.data.frame <- function(x) {
    cns <- c("Cholesterol", "Gene", "Class")
    if (!(all(colnames(x) %in% cns)))
        stop(paste("Column names are not ", cns))
    tmp <- x[, cns]
    ## Do you know why do I preserve the data.frame class? ...
    class(tmp) <- c("Cholest_Gene", class(tmp)) ## "data.frame")
    return(tmp)
}

uu <- to_CG(data.frame(Cholesterol = 1:10, Gene = 11:20,
                 Class = "Cl1"))

print.Cholest_Gene <- function(x) {
    u <- x[, c(1, 2)]
    class(u) <- "data.frame"
    print(u)
    cat("\n hola \n")
    print(summary(x[, 1]))
}


uu
summary(uu)
str(uu)
attributes(uu)

to_CG(cbind(Cholesterol = 1:10, Gene = 11:20))

## but ugly? write a default that will fail

to_CG.default <- function(x) {
    stop("For now, only methods for data.frame are available.")
}


to_CG(cbind(Cholesterol = 1:10, Gene = 11:20))


## Now, test!!!!!!!!!!!! Really, do.
## at least:
##  - that we can create a legitimate one from a data.frame
##  - that it fails as we expect when data.frame with missing columns
##  - that it fails as we expect when not a data.frame

library(testthat)

test_that("minimal conversions and failures", {

    expect_s3_class(to_CG(data.frame(Cholesterol = 1:10, Gene = 11:20,
                                     Class = "Cl1")), "Cholest_Gene")

    expect_error(to_CG(cbind(Cholesterol = 1:10, Gene = 11:20)),
                 "For now, only methods for data.frame are available",
                 fixed = TRUE)

    expect_error(to_CG(data.frame(Cholesterol = 1:10, Geni = 11:20,
                                     Class = "Cl1")),
                 "Column names are not",
                 fixed = TRUE)
})

## eh? This fails! And the output is ugly!!
expect_s3_class(to_CG(data.frame(Cholesterol = 1:10, Gene = 11:20,
                                 Class = "Cl1",
                                 whatever = "abcd")), "Cholest_Gene")

## Fix the code
to_CG.data.frame <- function(x) {
    cns <- c("Cholesterol", "Gene", "Class")
    if (!(all(cns %in% colnames(x))))
        stop(paste("Column names are not ", paste(cns, collapse = " ")))
    tmp <- x[, cns]
    ## Do you know why do I preserve the data.frame class? ...
    class(tmp) <- c("Cholest_Gene", "data.frame")
    return(tmp)
}

test_that("minimal conversions and failures", {
    expect_s3_class(to_CG(data.frame(Cholesterol = 1:10, Gene = 11:20,
                                     Class = "Cl1")), "Cholest_Gene")
    expect_error(to_CG(cbind(Cholesterol = 1:10, Gene = 11:20)),
                 "For now, only methods for data.frame are available",
                 fixed = TRUE)
    expect_error(to_CG(data.frame(Cholesterol = 1:10, Geni = 11:20,
                                     Class = "Cl1")),
                 "Column names are not",
                 fixed = TRUE)
    expect_s3_class(to_CG(data.frame(Cholesterol = 1:10, Gene = 11:20,
                                     Class = "Cl1",
                                     whatever = "abcd")), "Cholest_Gene")
})



plot.Cholest_Gene <- function(x, ...) {
    class(x) <- "data.frame"
    require(ggplot2)
    ## FIXME: should I explicitly print? Hummm.. return, as orthodox?
    if (nlevels(x$Class) >= 2 )
        p1 <- ggplot(aes(y = Cholesterol, x = Gene, col = Class), data = x) +
            facet_grid(~ Class)
    else
        p1 <- ggplot(aes(y = Cholesterol, x = Gene), data = x)
    p1 <- p1 + geom_point()
    return(p1)
}

plot(uu)

uu2 <- uu
uu2[1:3, "Class"] <- "Cl2"
## notice the NA, but that ain't related to our class
uu2$Class <- as.character(uu2[, "Class"])
uu2[1:3, "Class"] <- "Cl2"
uu2[, "Class"] <- factor(uu2[, "Class"])

plot(uu2)




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
showMethods("print")
print
## From PatrickBurns: http://www.burns-stat.com/r-navigation-tools/
## However S3 generics can mutate to be both S3 and S4 generic:

print(methods(class = "Matrix"))
print(methods(class = "Matrix"), byclass = FALSE)
print(methods("dim"), byclass = FALSE)  

getAnywhere("symmpart")
getAnywhere("dim")

showMethods("Cholesky")
getAnywhere("Cholesky")

## eh? 
showMethods("Diagonal")
getAnywhere("Diagonal")

## look at the help for both Diagonal and Cholesky

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

## If you’ve programmed in a mainstream OO language, RC will seem very
## natural. But because they can introduce side effects through mutable
## state, they are harder to understand.

getClass(class(m1))
getClass(class(fit1))
getClass("list")
