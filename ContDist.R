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
