#########################################################
#                                                       #
#                 Continuous Dist.                      #
#                                                       #
#########################################################
library(tidyverse)

?dnorm
y1 <- dnorm(x, 0, 1)
y2 <- dnorm(x, 0, 3)
y3 <- dnorm(x, 3, 1)
y4 <- dnorm(x, 2, 3)

plot(x,y1)
lines(x,y2,'red')

?dgamma
sh1 <- 1
sc1 <- 1
sh2 <- 2
sc2 <- 0.33

x <- seq(0,25,0.1)
prob1 <- dgamma(x,shape = sh1,scale = sc1)
prob2 <- dgamma(x,sh2,sc1)

plot(x, prob1, type = "1")
lines(x, prob2, col = "red")


p1 <- dbeta(x, shape1 = shape1, shape2 = shape2)
p2 <- dbeta(x, shape1 = shape1_2, shape2 = shape2_2)
p3 <- dbeta(x, shape1 = shape1_3, shape2 = shape2_3)

