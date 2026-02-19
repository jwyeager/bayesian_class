##################################################
#                 Bayesian Lab 1                 #
##################################################
# 1) Generate large numbers of random samples from a particular distribution. Here we simulate
# 10,000 samples of size n=50 from a normal distribution with mean of 1 and standard deviation
# of 2

n <- 50
mu <- 1
sd <- 2

# 2) Calculate mean for each sample.
# mean.X <- apply(X, 1, “mean”)
x <- rnorm(500000,mu,sd)
x <- matrix(x, nrow = 10000, ncol = 50)
x.bar <- apply(x, 1, 'mean')

# 4) Calculate mean and standard deviation of the sample means and plot histogram of sample
# means. Then discuss whether the simulated samples follow CLT.
x.bar.i <- mean(x.bar)
sd.x.bar <- sd(x.bar)

hist(x.bar)

qqnorm(x.bar)
qqline(x.bar, col = "red")

# yes, these data follow CLT as they appear to be normally distributed around mu = 1
# upon visual inspection of histogram, further supported by Q-Q plot.

# 4) We just test CLT using simulated data from a normal distribution. What if the underlying
# distribution is not normal distribution? Test CLT using random samples from gamma distribution
# with shape parameter equal to 2 and scale parameter equal to 3. Check the notes for calculating
# mean and standard deviation of gamma distribution from its shape and scale parameters.

a <- 2
s <- 3
r <- 1 / s

X <- rgamma(500000, shape = a, scale = s)
X <- matrix(X, nrow = 10000, ncol = 50)

(X.bar <- a / r) #[1] 6
(X.sd <- a * s^2) #[1] 18

# for normally distributed data, the minimum sample size for CLT to apply would be 3, but 
# for this gamma distribution as an example, first find the skewness with shape = 2:
(skew <- 2 / sqrt(2)) #[1] 1.414214

# 1.41 is right skewed, and would look reasonable symmetric at skewness = 0.5, so solve
# for n 

(sqrt.n <- (sqrt(2)) / 0.5) # [1] 2.828427

(n <- sqrt.n^2) #[1] 8

# thus the minimum sample size for a gamma distribution where shape = 2 would be n = 8,
# however a larger sample size, say n = 30, would be more widely acceptable
