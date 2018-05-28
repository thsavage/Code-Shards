# Flip a coin 10 times.
# p is the support of the outcome.
# p.true is the true probabilty of heads.  Move away from 0.5 for an unbalanced coin.
# N is the number of flips.  Increase if you want a larger more experimental outcomes (sample size).
# Flip are generated from binomial RV with parameter p.true.
# Table shows the outcomes.
p <- seq(from=0.005, to=0.995, by=0.005)  
p.true <- 0.5  
N <- 10  
y <- rbinom(N, size=1, prob=p.true)  
table(y)  

# Set the hyperparameters on the beta prior.  1,1 is uniform.  10,10 is bell shaped.  .1, .1 is Jeffrey's prior.
alpha.prior <- 1
beta.prior <- 1

# This is the posterior distribution as beta(alpha.prior, beta.prior)  
plot(p, dbeta(p, alpha.prior, beta.prior),
     col="blue", lwd=2, type="l", las=1, bty="n", 
     ylim=c(0, 8), ylab="Density", xlab="Support for P(Heads)",
     main="Prior Probability Model for Coin Flip")

# Graph the likelihood of the observed outcomes assuming the binomial distribution.
# Note that I rescale the likelihood so that it can be graphed together with prior and posterior.
likelihood <- sapply(p, function(p) { prod(p^y * (1-p)^(1-y)) } )
# plot(p, likelihood, lwd=2, las=1, bty="n", type="l", xlab="Coin Flip", ylab="Likelihood")
like.rescale <- N * p.true * likelihood / max(likelihood)  ## This rescales the likelihood for visual presentation.
plot(p, like.rescale, lwd=2, las=1, bty="n", type="l", xlab="Coin Flip", ylab="Likelihood")

# Graph everything together.  Note how the likelihood "reshapes" the prior.
plot(p, like.rescale, lwd=2, las=1, bty="n", type="l", 
     main="Posterior (red) is Prior (blue) x Likelihood (black)", xlab="Coin Flip", ylab="Likelihood")
lines(p, dbeta(p, alpha.prior, beta.prior), col="blue", lwd=2)
alpha.post <- alpha.prior + sum(y)
beta.post <- beta.prior + sum(1-y)
lines(p, dbeta(p, alpha.post, beta.post), col="red", lwd=2)


# Bayesian Inference Using Coin Flipping Experiment
# Must have R Studio for this code to work
# If you understand this visual, then you will understand the core of the Bayesian concept of 
# Prior Probability
library(manipulate)
p <- seq(from=0.005, to=0.995, by=0.005)
manipulate( 
  {plot(p, dbeta(p, alpha.hyper, beta.hyper), 
        col="blue", lwd=2, type="l", las=1, bty="n", 
        ylim=c(0, 8), ylab="Density", xlab="Pr(Heads)",
        main="Prior Probability")
    polygon(c(p, rev(p)), c(dbeta(p, alpha.hyper, beta.hyper), 
                            rep(0, length(p))), col=rgb(0, 0, 1, 0.2), border=NA)}, 
  alpha.hyper=slider(0.1, 10, step=0.1, initial=1), 
  beta.hyper=slider(0.1, 10, step=0.1, initial=1))


# Flip a coin 10 times
set.seed(12345)
p <- seq(from=0.005, to=0.995, by=0.005)  # Support for outcome
p.true <- 0.5  # This is the true probability of heads.  Increase if you want an unbalanced coin.
N <- 10  # Number of flips.  Increase if you want a larger set of outcomes
y <- rbinom(N, size=1, prob=p.true)  # Generate flips following binomial random variable with parameter p.
table(y) # Outcomes

# Graph the likelihood of the observed outcomes given a fair coin assuming the binomial distribution.
likelihood <- sapply(p, function(p) { prod(p^y * (1-p)^(1-y)) } )
# plot(p, likelihood, lwd=2, las=1, bty="n", type="l", xlab="Coin Flip", ylab="Likelihood")
like.rescale <- N * p.true * likelihood/max(likelihood)  # This rescales the likelihood for visual presentation.
plot(p, like.rescale, lwd=2, las=1, bty="n", type="l", xlab="Coin Flip", ylab="Likelihood", main="Likelihood of Observed Data")


# Examine posterior using outcomes from the flips.  Prior and posteriors are conjugate beta distributions.
manipulate(
  {plot(p, like.rescale, lwd=2, las=1, bty="n", 
        ylim=c(0,8), type="l", ylab="Density", xlab="",
        main="Posterior (red) is proportional to Likelihood (black) x Prior (blue)")
    alpha.hyper.post <- alpha.hyper + sum(y)
    beta.hyper.post <- beta.hyper + N - sum(y)
    lines(p, dbeta(p, alpha.hyper, beta.hyper), col="blue", lwd=2)
    polygon(c(p, rev(p)), c(dbeta(p, alpha.hyper, beta.hyper), 
                            rep(0, length(p))), col=rgb(0, 0, 1, 0.2), border=NA)
    lines(p, dbeta(p, alpha.hyper.post, beta.hyper.post), col="red", lwd=2)
    polygon(c(p, rev(p)), c(dbeta(p, alpha.hyper.post, beta.hyper.post), 
                            rep(0, length(p))), col=rgb(1, 0, 0, 0.2), border=NA)
    lines(p, like.rescale, lwd=2)}, 
  alpha.hyper=slider(0.1, 10, step=0.1, initial=1), 
  beta.hyper=slider(0.1, 10, step=0.1, initial=1))