# Script for the first example of the FWE paper

require(RelDists)
require(gamlss)

# Data from Bebbington et al. (2007)
y <- c(2.160, 0.746, 0.402, 0.954, 0.491, 6.560, 4.992, 0.347,
       0.150, 0.358, 0.101, 1.359, 3.465, 1.060, 0.614, 1.921,
       4.082, 0.199, 0.605, 0.273, 0.070, 0.062, 5.320)

# FIGURE
pdf('Figs/hist_ecdf_esurv_example_1.pdf', width=12, height=12)
par(mfrow=c(2, 2))
# Histogram
hist(y, freq=FALSE, las=1, breaks=5, 
     main="(a)", ylab="Relative frequency", col="white", border=gray(0.8))
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
     ylab="Empirical h(y)", xlab="y",
     xlim=c(0, 7), ylim=c(0, 2.5))

dev.off()


# Base model --------------------------------------------------------------

library(RelDists)
library(gamlss)
mod <- gamlss(y~1, family='FWE')

summary(mod)

exp(coef(mod, what="mu"))
exp(coef(mod, what="sigma"))
AIC(mod)


# Choosing the distribution between Realplus ------------------------------
ch <- chooseDist(object=mod, type="realplus", 
                 k=2, extra=c("FWE"))

ch <- na.omit(ch)
ch

orden <- order(ch[, 1])
ch <- ch[orden, , drop = FALSE]
nombres_ordenados <- rownames(ch)
rownames(ch) <- nombres_ordenados
ch


# Fitting 3 models --------------------------------------------------------

# To use the robust version of gamlss
source("https://raw.githubusercontent.com/fhernanb/dist_gamlss_book/main/%5B03%5D%20Robust%20estimation%20example/gamlssRobust.R")

# FWE
mod_rob <- gamlssRobust(mod, 
                        bound=2.878162, 
                        CD.bound=3.208707, 
                        trace=FALSE)

mod_rob <- gamlssRobust(mod, trace=FALSE)

summary(mod_rob)

exp(coef(mod_rob, what="mu"))
exp(coef(mod_rob, what="sigma"))
AIC(mod_rob)

# LOGNO
mod_LOGNO <- gamlss(y~1, family='LOGNO')
summary(mod_LOGNO)
exp(coef(mod_LOGNO, what="mu"))
exp(coef(mod_LOGNO, what="sigma"))
AIC(mod_LOGNO)

mod_LOGNO_rob <- gamlssRobust(mod_LOGNO, trace=FALSE)
summary(mod_LOGNO_rob)
exp(coef(mod_LOGNO_rob, what="mu"))
exp(coef(mod_LOGNO_rob, what="sigma"))
AIC(mod_LOGNO_rob)

# IG
mod_IG <- gamlss(y~1, family='IG')
summary(mod_IG)
exp(coef(mod_IG, what="mu"))
exp(coef(mod_IG, what="sigma"))
AIC(mod_IG)

mod_IG_rob <- gamlssRobust(mod_IG, trace=FALSE)
summary(mod_IG_rob)
exp(coef(mod_IG_rob, what="mu"))
exp(coef(mod_IG_rob, what="sigma"))
AIC(mod_IG_rob)

# Applying Kolmogorov-Smirnov test
ks.test(x=y, "pFWE", mu=0.2050, sigma=0.2589)
ks.test(x=y, "pFWE", mu=0.2230, sigma=0.3132)

ks.test(x=y, "pLOGNO", mu=0.7067, sigma=1.3646)
ks.test(x=y, "pLOGNO", mu=0.7104, sigma=1.3556)

ks.test(x=y, "pIG", mu=1.5779, sigma=1.6245)
ks.test(x=y, "pIG", mu=1.5809, sigma=1.4776)


# The fitted values
mu_hat  <- exp(coef(mod_rob, what="mu"))
sig_hat <- exp(coef(mod_rob, what="sigma"))

mu_hat
sig_hat

# FIGURE
pdf('Figs/res_example_1.pdf', width=12, height=12)

par(mfrow=c(2, 2))

# f(y)
hist(y, freq=FALSE, las=1, breaks=5, main="(a)", ylim=c(0, 0.6), 
     ylab="f(y)", xlab="y", col="white", border=gray(0.8))
curve(dFWE(x, mu = mu_hat, sigma = sig_hat), from=0, to=7, ylim=c(0,1),
      col="black", las=1, lwd=2, add=TRUE)

# F(y)
plot(ecdf(x=y), ylim=c(0,1), main="(b)", las = 1, ylab="F(y)", xlab="y", 
     col=gray(0.8), xaxt='n')
axis(side=1, at=0:7, labels=0:7)

# S(y)
curve(pFWE(x, mu = mu_hat, sigma = sig_hat, lower.tail = TRUE, log.p = FALSE),
      from = 0, to = 7, col="black", lwd=2, add=TRUE, main="(c)")

fun <- ecdf(y)
sfun0  <- stepfun(x=sort(y), y=c(1, 1-fun(sort(y))), right=FALSE)
plot(sfun0, pch = 19, verticals = FALSE, las=1,
     main="(c)", ylab="R(y)", xlab="y", col=gray(0.8), xaxt='n')
axis(side=1, at=0:7, labels=0:7)
abline(h = c(0, 1), col = "gray70", lty = 2)
curve(pFWE(x, mu = mu_hat, sigma = sig_hat, lower.tail = FALSE, log.p = FALSE),
      from = 0, to = 7, col="black", lwd=2, las = 1, ylab="R(y)", xlab="y",
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
     ylab="Empirical h(y)", xlab="y",
     xlim=c(0, 7), ylim=c(0, 2.5))

curve(hFWE(x, mu = mu_hat, sigma = sig_hat), from=0, to=7, ylim=c(0, 2.5), 
      col="black", lwd=2, las=1, ylab="H(y)", xlab="y", add=TRUE)

dev.off()


