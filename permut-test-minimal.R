######## What we need (wish list)   #############

## Function to compute difference of means
## Randomly assign data to groups
## Repeat many times






## two example data sets
d21 <- rnorm(13)
d22 <- d21[1:9] + 3


mean.d <- function(x1, x2)
    mean(x1) - mean(x2)

mean.d(d21, d22)

sampleAndStat <- function(x1, x2) {
    tmp <- sample(c(x1, x2))
    g1 <- tmp[seq(x1)]
    g2 <- tmp[seq(from = length(x1) + 1,
                  to = length(tmp))]
    return(mean.d(g1, g2))
}


sampleAndStat(d21, d22)

set.seed(1)
summary(replicate(100, sampleAndStat(d21, d22)))

set.seed(1)
manyV <- replicate(100, sampleAndStat(d21, d22))
summary(manyV)


manyV <- replicate(1000, sampleAndStat(d21, d22))
hist(manyV)
hist(manyV, xlim = c(-3, 3))

## note the canvas analogy!
abline(v = mean.d(d21, d22))

## more canvas :-)
hist(manyV, xlim = c(-3, 3))
abline(v = mean.d(d21, d22), col = "red")


permut.test <- function(data1, data2, 
                        num.permut = 100) {
  obs.stat <- mean.d(data1, data2)
  permut.stat <- replicate(num.permut, 
                           sampleAndStat(data1, data2))
  pv <- (sum(abs(permut.stat) >= abs(obs.stat)) + 
         1) / (num.permut + 1)
  message("\n p-value is ", pv, "\n")
  hist(permut.stat, xlim = c(min(obs.stat, 
                      min(permut.stat)),
                      max(obs.stat, 
                          max(permut.stat))))
  abline(v = obs.stat, col = "red")
  return(list(obs.stat = obs.stat,
              permut.stat = permut.stat,
              pv = pv))
}

permut.test(d21, d22)


## Nicer labelling
permut.test.2 <- function(data1, data2, 
                        num.permut = 100) {
  obs.stat <- mean.d(data1, data2)
  permut.stat <- replicate(num.permut, 
                           sampleAndStat(data1, data2))
  pv <- (sum(abs(permut.stat) >= abs(obs.stat)) + 
         1) / (num.permut + 1)
  message("\n p-value is ", pv, "\n")
  title <- paste(deparse(substitute(data1)), "-",
                 deparse(substitute(data2)))
  subtitle <- paste0("Distribution of permuted statistic.",
           " In red, observed one.") 
  hist(permut.stat, xlim = c(min(obs.stat, 
                      min(permut.stat)),
                      max(obs.stat, 
                          max(permut.stat))),
       main = title, xlab = "", sub = subtitle)
  abline(v = obs.stat, col = "red")
  return(list(obs.stat = obs.stat,
              permut.stat = permut.stat,
              pv = pv))
}


permut.test.2(d21, d22)


## Now: rename functions if needed, add tests, comment things properly.


## Oh, and what about the t.test

plot(function(x) dt(x, df = 2), -3, 3, 
     main = "t: Density",
     ylab = "Density",
     ylim = c(0, 0.42))


x1 <- seq(-3, 3, length.out = 500)
y1 <- sapply(x1, function(x) dt(x, df = 1000))
lines(x1, y1, col = "blue")


