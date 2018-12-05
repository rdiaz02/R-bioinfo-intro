## wish list

## \begin{enumerate}
## \item Compute our statistic (the difference in means here).
## \item Generate data distributions according to the null hypothesis.
## \item Compute the statistic for each new data configuration.
## \item Compare what we compute from our original data with what we obtain
##   under the null ($H_o$).
## \end{enumerate}



## difference of means

## random assign elem. to groups
##    - and find diff in means

## repeat the above many times

## compute p-value

## plot

d11 <- rnorm(15)
d12 <- d11[1:7]
# difference
d21 <- rnorm(13)
d22 <- d21[1:9] + 3


mean.d <- function(x, y) mean(x) - mean(y)

## permut_mean_1 <- function(x1, x2) {
##     g1 <- sample(c(x1, x2), size = length(x1),
##                  replace = FALSE)
##     g2 <- setdiff(c(x1, x2), g1)
##     return(mean.d(g1, g2))
## }

## why is that not a good idea?
## ## FIXME setdiff si hay elementos comunes??  como lo pillamos? comparar
## ## tamaños grupos dentro código

## permut_mean_1(x1, x2)



## two vectors of numbers ->
##                  difference between means of vectors after permutation
##   we sample blablabal
permut_mean_2 <- function(x1, x2) {
    g0 <- sample(c(x1, x2),  replace = FALSE)
    g1 <- g0[seq(x1)]
    g2 <- g0[seq(from = length(x1) + 1,
                 to = length(g0))]
    return(mean.d(g1, g2))
}


permut_test_1 <- function(x1, x2, fun_permut = permut_mean_2,
                          num.permut = 100) {
    stat_obs <- mean.d(x1, x2)
    stat_permut <- replicate(num.permut, fun_permut(x1, x2))

    pv <- (1/num.permut) * sum( abs(stat_obs) < abs(stat_permut) )

    message("\n p-value is ", pv, "\n")
    list(
        pv = pv,
        stat_obs = stat_obs,
        stat_permut = stat_permut
    )
}

## wrong!
stopifnot(permut_test_1(1:3, 40:50, num.permut = 1000)$pv < 1)
stopifnot(permut_test_1(rep(1, 3), rep(1, 5), num.permut = 1000)$pv == 1)
stopifnot(permut_test_1(d11, d11, num.permut = 1000)$pv == 1)



library(testthat)

test_that("p-value is 1 when the same vector in both positions", {
    n <- 100
    for(i in 1:n) {
        set.seed(i); cat("\n doing i = ", i, "\n")
        rxx <- rnorm(20)
        expect_true(permut_test_1(rxx, rxx, num.permut = 1000)$pv == 1)
    }
})


test_that("p-value is 1 when same elements, different length", {
    n <- 10
    for(i in 1:n) {
        e <- runif(1)
        rx1 <- rep(e, 5)
        rx2 <- rep(e, 15)
        expect_true(permut_test_1(rx1, rx2, num.permut = 1000)$pv == 1)
    }
})

test_that("p-value is < 1 when extreme differences", {
    n <- 10
    numcases <- 100
    for(i in 1:n) {
        rxx <- runif(numcases)
        rxy <- rxx + 1000
        expect_true(permut_test_1(rxx, rxy, num.permut = 1000)$pv < 1)
    }
})


## fix the function

## 
## 

permut_test_1 <- function(x1, x2, fun_permut = permut_mean_2,
                          num.permut = 100) {
    stat_obs <- mean.d(x1, x2)
    stat_permut <- replicate(num.permut, fun_permut(x1, x2))

    pv <- (1/num.permut) * sum( abs(stat_obs) <= abs(stat_permut) )

    message("\n p-value is ", pv, "\n")
    list(
        pv = pv,
        stat_obs = stat_obs,
        stat_permut = stat_permut
    )
}


stopifnot(permut_test_1(1:3, 40:50, num.permut = 1000)$pv < 1)
stopifnot(permut_test_1(rep(1, 3), rep(1, 5), num.permut = 1000)$pv == 1)
stopifnot(permut_test_1(d11, d11, num.permut = 1000)$pv == 1)



test_that("p-value is 1 when the same vector in both positions", {
    n <- 10
    for(i in 1:n) {
        rxx <- rnorm(20)
        expect_true(permut_test_1(rxx, rxx, num.permut = 1000)$pv == 1)
    }
})

test_that("p-value is 1 when same elements, different length", {
    n <- 10
    for(i in 1:n) {
        e <- runif(1)
        rx1 <- rep(e, 5)
        rx2 <- rep(e, 15)
        expect_true(permut_test_1(rx1, rx2, num.permut = 1000)$pv == 1)
    }
})


test_that("p-value is < 1 when extreme differences", {
    n <- 10
    numcases <- 100
    for(i in 1:n) {
        rxx <- runif(numcases)
        rxy <- rxx + 1000
        expect_true(permut_test_1(rxx, rxy, num.permut = 1000)$pv < 1)
    }
})

## technical improvement

permut_test_1 <- function(x1, x2, fun_permut = permut_mean_2,
                          num.permut = 100) {
    stat_obs <- mean.d(x1, x2)
    stat_permut <- replicate(num.permut, fun_permut(x1, x2))

    pv <- (1/(num.permut + 1)) * (sum( abs(stat_obs) <= abs(stat_permut) ) + 1)

    message("\n p-value is ", pv, "\n")
    list(
        pv = pv,
        stat_obs = stat_obs,
        stat_permut = stat_permut
    )
}



test_that("p-value is 1 when the same vector in both positions", {
    n <- 10
    for(i in 1:n) {
        rxx <- rnorm(20)
        expect_true(permut_test_1(rxx, rxx, num.permut = 1000)$pv == 1)
    }
})

test_that("p-value is 1 when same elements, different length", {
    n <- 10
    for(i in 1:n) {
        e <- runif(1)
        rx1 <- rep(e, 5)
        rx2 <- rep(e, 15)
        expect_true(permut_test_1(rx1, rx2, num.permut = 1000)$pv == 1)
    }
})


test_that("p-value is < 1 when extreme differences", {
    n <- 10
    numcases <- 100
    for(i in 1:n) {
        rxx <- runif(numcases)
        rxy <- rxx + 1000
        expect_true(permut_test_1(rxx, rxy, num.permut = 1000)$pv < 1)
    }
})



## add plotting code

## two vectors of numbers -> p-value of mean diff from permut.test,
##                           (list with p-value, permut ...)
##                         side effects: histogram of differences blablabl
permut_test_1 <- function(x1, x2, fun_permut = permut_mean_2,
                          num.permut = 100,
                          plot = TRUE) {
    stat_obs <- mean.d(x1, x2)
    stat_permut <- replicate(num.permut, fun_permut(x1, x2))

    pv <- (1/(num.permut + 1)) * (sum( abs(stat_obs) <= abs(stat_permut) ) + 1)

    message("\n p-value is ", pv, "\n")
    if(plot) {
        title <- paste(deparse(substitute(x1)), "-",
                       deparse(substitute(x2)))
        ## title <- paste(x1 - x2)
        subtitle <- paste0("Distribution of permuted statistic.",
                           " In red, observed one.") 
        hist(stat_permut, xlim = c(min(stat_obs, 
                                       min(stat_permut)),
                                   max(stat_obs, 
                                       max(stat_permut))),
             main = title, xlab = "", sub = subtitle)
        abline(v = stat_obs, col = "red")
    }
    list(
        pv = pv,
        stat_obs = stat_obs,
        stat_permut = stat_permut
    )
}

permut_test_1(d11, d12)

permut_test_1(d11, d11 + 5)


## We would test again with plot = TRUE and plot = FALSE

## And add function signatures

