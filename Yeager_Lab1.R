##################################################
#                 Bayesian Lab 1                 #
##################################################
n <- 50
mu <- 1
sd <- 2

x <- rnorm(500000,mu,sd)
x <- matrix(x, nrow = 10000, ncol = 50)
x.bar <- apply(x, 1, 'mean')

x.bar.i <- mean(x.bar)
sd.x.bar <- sd(x.bar)

hist(x.bar)

qqnorm(x.bar)
qqline(x.bar, col = "red")

# yes, these data follow CLT as they appear to be normally distributed around mu = 1
# upon visual inspection of histogram, further supported by Q-Q plot.

X <- rgamma(500000, shape = 2, scale = 3)
X <- matrix(X, nrow = 10000, ncol = 50)
X.bar <- apply(X, 1, 'mean')
X.bar.i <- mean(X.bar)
sd.X.bar <- sd(X.bar)



