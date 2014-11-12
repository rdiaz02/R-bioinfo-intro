randomdata <- matrix(rnorm(50 * 500), ncol = 50)
clase <- factor(c(rep("sano", 20), rep("enfermo", 30)))
tmp <- t.test(randomdata[1, ] ~ clase)
tmp
attributes(tmp)
tmp$p.value
pvalues <- apply(randomdata, 1, function(x) t.test(x ~ clase)$p.value)
hist(pvalues)
order(pvalues)
which(pvalues < 0.05)
sum(pvalues < 0.05)
