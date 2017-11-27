## Functions that return functions with
## bindings for some arguments

powb <- function(n) {
    return(function(x) x^n)
}

cc <- powb(3)
cc(2)

as.list(environment(cc))
## ls(envir = environment(cc))

## not that helpful
as.list(environment(powb))

## lazy eval
fab <- function(a, b) 2 * a
fab(3) ## no error

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
## but two is being passed also in ...
## debug and do list(...)

f5(1:5, two = 1:5, col = "red")




