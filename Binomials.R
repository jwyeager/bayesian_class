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
