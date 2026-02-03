#########################################################
#                                                       #
#                   Binomial Dist.                      #
#                                                       #
#########################################################
library(tidyverse)

# Bernoulli
# mean expected value = probability of success

?rbinom
n <- 10
p1 <- 0.1
p2 <- 0.5
p3 <- 0.9
x <- rbinom(1000, n, p1)
y <- dbinom(x, n, p1)
x <- seq(0,10)
barplot(y~x)

pbinom(4, n, p1) #[1] 0.9983651
qbinom(0.3, n, p1) #[1] 0
qbinom(0.9983651, n, p1) #[1] 5

y2 <- dbinom(x, n, p2)
barplot(y2 ~ x)

#########################################################
#                                                       #
#                    Poisson Dist.                      #
#                                                       #
#########################################################

# example plots
lam <- c(0.8,3,12)
rpois(100,lam[1])
rpois(100,lam[2])
rpois(100,lam[3])

qpois(0.5,lam[3]) #[1] 12

ppois(12,lam[3]) #[1] 0.5759652

x <- seq(0,20)

prb <- matrix(NA, 3, 21) # must have initial values 

for (i in 1:3) {
  prb[i,] <- dpois(x, lam[i])
}
barplot(prb[1,] ~ x)
barplot(prb[2,] ~ x)
barplot(prb[3,] ~ x)

#########################################################
#                                                       #
#                 Negative Binomial                     #
#                                                       #
#########################################################
?NegBinomial

rnbinom(100, size = 0.1, mu = 2)
x <- seq(0, 10)
# y <- dnbinom(x, )
# k <- c(0.1, 1, 10)
# mu <- 2

#########################################################
#                                                       #
#                      Geometric                        #
#                                                       #
#########################################################
rgeom(100, 0.2)
x <- seq(0,20)
y <- dgeom(x, 0.2)
barplot(y~x)
y2 <- dgeom(x,0.8)
barplot(y2~x)


################################
n <- 10
p <- c(0.1,0.2,0.3)
x <- seq(0,10)
prob <- matrix(NA, 3, 11)

for (i in 1:3) {
  prob[i,] <- dbinom(x,n,p[i])
}
barplot(prob[1,] ~ x)
barplot(prob[2,] ~ x)
barplot(prob[3,] ~ x)
