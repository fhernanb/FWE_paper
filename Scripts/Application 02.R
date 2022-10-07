# Script for the second example in the FWE paper

require(RelDists)
require(gamlss)

# Data from Bader and Priest (1982)
# https://www.researchgate.net/publication/320309784_Flexible_Weibull_Distribution
y <- c(1.901, 2.132, 2.203, 2.228, 2.257, 2.350, 2.361, 2.396, 2.397, 
       2.445, 2.454, 2.474, 2.518, 2.522, 2.525, 2.532, 2.575, 2.614, 
       2.616, 2.618, 2.624, 2.659, 2.675, 2.738, 2.740, 2.856, 2.917, 
       2.928, 2.937, 2.937, 2.977, 2.996, 3.030, 3.125,3.139, 3.145, 
       3.220, 3.223, 3.235, 3.243, 3.264, 3.272, 3.294, 3.332, 3.346, 
       3.377, 3.408, 3.435, 3.493, 3.501, 3.537,3.554, 3.562, 3.628, 
       3.852, 3.871, 3.886, 3.971, 4.024, 4.027, 4.225, 4.395, 5.020) 

# FIGURE
pdf('Figs/hist_ecdf_esurv_example_2.pdf', width=12, height=12)
par(mfrow=c(2, 2))
# Histogram
hist(y, freq=FALSE, las=1, xlim=c(1, 6),
     main="(a)", ylab="Relative frequency", col="white")
rug(x=y, col=gray(0.1))
# Empirical cdf
plot(ecdf(x=y), las=1, xlim=c(1, 6),
     main="(b)", ylab="Empirical F(y)", xlab="y", col=gray(0.8), xaxt='n')
axis(side=1, at=1:6, labels=1:6)
# Empirical surv
fun <- ecdf(y)
sfun0  <- stepfun(x=sort(y), y=c(1, 1-fun(sort(y))), right=FALSE)
plot(sfun0, pch = 19, verticals = FALSE, las=1, xlim=c(1, 6),
     main="(c)", ylab="Empirical R(y)", xlab="y", col=gray(0.8), xaxt='n')
axis(side=1, at=1:6, labels=1:6)
abline(h = c(0, 1), col = "gray70", lty = 2)
# Empirical hazard
esf <- function (x) {
  x <- sort(x)
  n <- length(x)
  if (n < 1) 
    stop("'x' must have 1 or more non-missing values")
  vals <- unique(x)
  fun <- ecdf(x)
  rval <- stepfun(x=sort(x), y=c(1, 1-fun(x)), right=FALSE)
  class(rval) <- c("ecdf", "stepfun", class(rval))
  assign("nobs", n, envir = environment(rval))
  attr(rval, "call") <- sys.call()
  rval
}

emp_sur <- esf(y)
emp_pdf <- with(density(y), approxfun(x, y, rule=1))

plot(x=sort(y), y=emp_pdf(sort(y)) / emp_sur(sort(y)), 
     las=1, col="gray70", type="s", main="(d)",
     ylab="Empirical H(y)", xlab="y",
     xlim=c(1, 6), ylim=c(0, 5))

dev.off()


# Fitting the model
library(RelDists)
library(gamlss)
mod <- gamlss(y~1, family='FWE')

summary(mod)
AIC(mod)

mu_hat  <- exp(coef(mod, what="mu"))
sig_hat <- exp(coef(mod, what="sigma"))

# Next we fit the model using
# gamlss robust
source("https://raw.githubusercontent.com/fhernanb/dist_gamlss_book/main/%5B03%5D%20Robust%20estimation%20example/gamlssRobust.R")

mod_rob <- gamlssRobust(mod, 
                        bound=2.878162, 
                        CD.bound=3.208707, 
                        trace=FALSE)

summary(mod_rob)

exp(coef(mod_rob, what="mu"))
exp(coef(mod_rob, what="sigma"))
AIC(mod_rob)

# We use the robust estimations 
mu_hat <- 0.8595109
sig_hat <- 9.193876

# FIGURE
pdf('Figs/res_example_2.pdf', width=12, height=12)

par(mfrow=c(2, 2))

# f(y)
hist(y, freq=FALSE, las=1, main="(a)", xlim=c(1, 6),
     ylab="f(y)", xlab="y", col="white")
curve(dFWE(x, mu=mu_hat, sigma=sig_hat), from=0, to=7, ylim=c(0, 1),
      col="tomato", las=1, lwd=2, add=TRUE)

# F(y)
plot(ecdf(x=y), ylim=c(0, 1), main="(b)", las=1, ylab="F(y)", xlab="y", 
     col=gray(0.8), xaxt='n', xlim=c(1, 6))
axis(side=1, at=seq(from=1, to=6, by=1), labels=seq(from=1, to=6, by=1))

# S(y)
curve(pFWE(x, mu=mu_hat, sigma=sig_hat, lower.tail=TRUE, log.p=FALSE),
      from=0, to=25, col="tomato", lwd=2, add=TRUE, main="(c)")

fun <- ecdf(y)
sfun0  <- stepfun(x=sort(y), y=c(1, 1-fun(sort(y))), right=FALSE)
plot(sfun0, pch=19, verticals=FALSE, las=1, xlim=c(1, 6),
     main="(c)", ylab="R(y)", xlab="y", col=gray(0.8), xaxt='n')
axis(side=1, at=seq(from=1, to=6, by=1), labels=seq(from=1, to=6, by=1))
abline(h=c(0, 1), col="gray70", lty=2)
curve(pFWE(x, mu=mu_hat, sigma=sig_hat, lower.tail=FALSE, log.p=FALSE),
      from=0, to=25, col="tomato", lwd=2, las=1, ylab="R(y)", xlab="y",
      add=TRUE)

# H(y)
esf <- function (x) {
  x <- sort(x)
  n <- length(x)
  if (n < 1) 
    stop("'x' must have 1 or more non-missing values")
  vals <- unique(x)
  fun <- ecdf(x)
  rval <- stepfun(x=sort(x), y=c(1, 1-fun(x)), right=FALSE)
  class(rval) <- c("ecdf", "stepfun", class(rval))
  assign("nobs", n, envir=environment(rval))
  attr(rval, "call") <- sys.call()
  rval
}

emp_sur <- esf(y)
emp_pdf <- with(density(y), approxfun(x, y, rule=1))

plot(x=sort(y), y=emp_pdf(sort(y)) / emp_sur(sort(y)), 
     las=1, col="gray70", type="s", main="(d)",
     ylab="Empirical H(y)", xlab="y",
     xlim=c(1, 6), ylim=c(0, 5))

curve(hFWE(x, mu=mu_hat, sigma=sig_hat), from=0, to=5, #ylim=c(0, 2.5), 
      col="tomato", lwd=2, las=1, ylab="H(y)", xlab="y", add=TRUE)

dev.off()



