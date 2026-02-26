# This code is for the last slide in lecture 6
# Lecture 6 is about components of Bayes theorem

y <- c(5,10,11,12,14,9,8,6) #data

#likelihood

step <- 0.01 # Give a small step for the purpose of [y] 

theta <- seq(1,20,step)

lik <- rep(NA, length(theta))

for (i in 1:length(theta)){
    lik[i] <- prod(dpois(y,theta[i])) #prod is used because you have more than 1 data point
}

#Or you can write a likelihood function
#likelihood <- function (data, para) {
#    lik <- rep(NA, length(para))
#    for (i in 1:length(para)){
#        lik[i] <- prod(dpois(data,para[i]))
#    }
#    return (lik)
#}

#lik2 <- likelihood(y,theta) #call the likelihood function

# provide parameters for the prior distribution

shape1 <- 0.001
rate1 <- 0.001

prior <- dgamma(theta,shape=shape1, rate=rate1) 
# You may consider gamma distribution for the prior distribution for theta 
# as it is a positive continuous random variable 

#calculate joint distribution
joint <- lik * prior

#calculate [y] area under the joint distribution 
sum.joint <- sum(joint*step)  

#calculate posterior distribution

posterior <- joint/sum.joint