
## Multiple alignment data

hit <- read.table("hit-table-500-text.txt")

## I want a histogram of alignment length and a scatterplot of score
## vs. identity

par(mfrow = c(1, 2)) ## two figures side by side
hist(hit[, 5], breaks = 50, xlab = "", main = "Alignment length")
plot(hit[, 13] ~ hit[, 3], xlab = "Percent. identity", 
     ylab = "Bit score")





## Birds and reptiles

## Metabolic rate vs body mass
load("anage.RData")
str(anage)
head(anage)
summary(anage)



library(car) ## make the car package available; this is NOT 
             ## installing it. It is making it available
scatterplot(Metabolic.rate..W. ~ Body.mass..g., log="xy", 
            data = anage)

scatterplot(Metabolic.rate..W. ~ Body.mass..g.|Class, log="xy", 
            data = anage)



plot(Metabolic.rate..W. ~ Body.mass..g., log="xy", data = anage)

plot(Metabolic.rate..W. ~ Body.mass..g., log="xy", 
     col = c("salmon", "darkgreen")[Class], data = anage)
legend(5, 5, legend = levels(anage$Class), 
       col = c("salmon", "darkgreen"),
       pch = 1)

## locator?

## Regression lines with abline and transforming variables

anage$logMetab <- log(anage$Metabolic.rate..W.)
anage$logBodyMass <- log(anage$Body.mass..g.)

birds <- anage[anage$Class == "Aves", ]

(lm1 <- lm(logMetab ~ logBodyMass, data = birds))

plot(logMetab ~ logBodyMass, data = birds)
abline(lm1)

scatterplot(logMetab ~ logBodyMass, data = birds)

scatterplot(Metabolic.rate..W. ~ Body.mass..g., data = birds, 
            log = "xy")

## Now, move to t-tests if time permits

## why such light covering of plots?
