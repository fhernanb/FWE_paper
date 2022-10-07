# Script for the first example in the FWE paper

require(RelDists)
require(gamlss)

# Data from Bebbington et al. (2007)
y <- c(2.160, 0.746, 0.402, 0.954, 0.491, 6.560, 4.992, 0.347,
       0.150, 0.358, 0.101, 1.359, 3.465, 1.060, 0.614, 1.921,
       4.082, 0.199, 0.605, 0.273, 0.070, 0.062, 5.320)

# FIGURE
pdf('hist_ecdf_esurv_example_1.pdf', width=12, height=12)
par(mfrow=c(2, 2))
# Histogram
hist(y, freq=FALSE, las=1, breaks=5, 
     main="(a)", ylab="Relative frequency", col="white")
rug(x=y, col=gray(0.1))
# Empirical cdf
plot(ecdf(x=y), las=1,
     main="(b)", ylab="Empirical F(y)", xlab="y", col=gray(0.8), xaxt='n')
axis(side=1, at=0:7, labels=0:7)
# Empirical surv
fun <- ecdf(y)
sfun0  <- stepfun(x=sort(y), y=c(1, 1-fun(sort(y))), right=FALSE)
plot(sfun0, pch = 19, verticals = FALSE, las=1,
     main="(c)", ylab="Empirical R(y)", xlab="y", col=gray(0.8), xaxt='n')
axis(side=1, at=0:7, labels=0:7)
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
     xlim=c(0, 7), ylim=c(0, 2.5))

dev.off()


# Fitting the model
library(RelDists)
library(gamlss)
mod <- gamlss(y~1, family='FWE')

summary(mod)
AIC(mod)

mu_hat  <- exp(coef(mod, what="mu"))
sig_hat <- exp(coef(mod, what="sigma"))

mu_hat
sig_hat

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

# FIGURE
pdf('res_example_1.pdf', width=12, height=12)

par(mfrow=c(2, 2))

# f(y)
hist(y, freq=FALSE, las=1, breaks=5, main="(a)", ylim=c(0, 0.6), 
     ylab="f(y)", xlab="y", col="white")
curve(dFWE(x, mu = mu_hat, sigma = sig_hat), from=0, to=7, ylim=c(0,1),
      col="tomato", las=1, lwd=2, add=TRUE)

# F(y)
plot(ecdf(x=y), ylim=c(0,1), main="(b)", las = 1, ylab="F(y)", xlab="y", 
     col=gray(0.8), xaxt='n')
axis(side=1, at=0:7, labels=0:7)

# S(y)
curve(pFWE(x, mu = mu_hat, sigma = sig_hat, lower.tail = TRUE, log.p = FALSE),
      from = 0, to = 7, col="tomato", lwd=2, add=TRUE, main="(c)")

fun <- ecdf(y)
sfun0  <- stepfun(x=sort(y), y=c(1, 1-fun(sort(y))), right=FALSE)
plot(sfun0, pch = 19, verticals = FALSE, las=1,
     main="(c)", ylab="R(y)", xlab="y", col=gray(0.8), xaxt='n')
axis(side=1, at=0:7, labels=0:7)
abline(h = c(0, 1), col = "gray70", lty = 2)
curve(pFWE(x, mu = mu_hat, sigma = sig_hat, lower.tail = FALSE, log.p = FALSE),
      from = 0, to = 7, col="tomato", lwd=2, las = 1, ylab="R(y)", xlab="y",
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
   assign("nobs", n, envir = environment(rval))
   attr(rval, "call") <- sys.call()
   rval
}

emp_sur <- esf(y)
emp_pdf <- with(density(y), approxfun(x, y, rule=1))

plot(x=sort(y), y=emp_pdf(sort(y)) / emp_sur(sort(y)), 
     las=1, col="gray70", type="s", main="(d)",
     ylab="Empirical H(y)", xlab="y",
     xlim=c(0, 7), ylim=c(0, 2.5))

curve(hFWE(x, mu = mu_hat, sigma = sig_hat), from=0, to=7, ylim=c(0, 2.5), 
      col="tomato", lwd=2, las=1, ylab="H(y)", xlab="y", add=TRUE)

dev.off()


