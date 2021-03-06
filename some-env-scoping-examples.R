## Functions that return functions with
## bindings for some arguments

powb <- function(n) {
    return(function(x) x^n)
}

cc <- powb(3)
cc(2)

ls(envir = environment(cc))

as.list(environment(cc))

## not that helpful
as.list(environment(powb))

## lazy eval
fab <- function(a, b) 2 * a
fab(3) ## no error



## What is in the ...?
## use list(...)
f6 <- function(one, two, ...) {
    plot(one, two, ...)
}

debugonce(f6)

f6(1:10, 1:10, col = "red", lty = 2)
## now, use list(...) inside


## More scoping
f2 <- function(x) {
    yy <- 5 + x
    return(yy)
}

f1 <- function(x) {
    x <- 3 * x
    z <- f2(x)
    3 * z
}

f1(2) ## x becomes 6, z becomes 11


rm(list = ls())

g1 <- function(x) x + y
y <- 10
g1(1)


g2 <- function(u) {
    y <- 100
    g1(u)
}

g2(1)

y <- 1000
g2(1)
## So g1 uses the y in the global env.,
## the place it was defined. Using the
## y inside g2 would have been dynamic scoping.


## Compare the above to

g4 <- function(u) {
    g5 <- function(x) x + y
    y <- 100 ## though clearer if before g5
    g5(u)
}

g4(1)

y <- -1000
g4(2)



## From a question in class, 2018-11-14:
##      what if there is no y defined in the environment
##      where g5 is defined?

g6 <- function(u) {
    g5 <- function(x) x + y
    ## y <- 100 ## No y in here
    g5(u)
}

## paranoid check: no g5
g5 ## ok, error

y <- 100
g6(1)

y <- 2
g6(1)

rm(y)
g6(1) ## fails



### Stop here

## Miscellaneous things not to do
f4 <- function(one, two, three = 3, four) {
    cat("one = ", one, 
        " two = ", two, 
        " three = ", three,
        " four = ", four,
        "\n")
}

## very confusing
f4(1, 4, 3, 2)

f5 <- function(one, ..., two) {
    plot(one, two, ...)
}
## won't work
f5(1:5, col = "red", 1:5)

## need this, but again bad practice
f5(1:5, col = "red", two = 1:5)

f5(1:5, two = 1:5, col = "red")



