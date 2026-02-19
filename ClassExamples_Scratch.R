y <- c(5,10,11,12,14,9,8,6)
step <- 0.01
theta <- seq(1,20,step)
lik <- rep(NA, length(theta))
for (i in 1:length(theta))