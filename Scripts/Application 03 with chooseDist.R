# Script for the third example of the FWE paper

require(RelDists)
require(gamlss)

# Data from Bader and Priest (1982)
# https://www.researchgate.net/publication/320309784_Flexible_Weibull_Distribution
y <- c(1.901, 2.132, 2.203, 2.228, 2.257, 2.350, 2.361, 2.396, 2.397, 
       2.445, 2.454, 2.474, 2.518, 2.522, 2.525, 2.532, 2.575, 2.614, 
       2.616, 2.618, 2.624, 2.659, 2.675, 2.738, 2.740, 2.856, 2.917, 
       2.928, 2.937, 2.937, 2.977, 2.996, 3.030, 3.125, 3.139, 3.145, 
       3.220, 3.223, 3.235, 3.243, 3.264, 3.272, 3.294, 3.332, 3.346, 
       3.377, 3.408, 3.435, 3.493, 3.501, 3.537, 3.554, 3.562, 3.628, 
       3.852, 3.871, 3.886, 3.971, 4.024, 4.027, 4.225, 4.395, 5.020) 

# FIGURE
pdf('Figs/hist_ecdf_esurv_example_2.pdf', width=12, height=12)
par(mfrow=c(2, 2))
# Histogram
hist(y, freq=FALSE, las=1, xlim=c(1, 6),
     main="(a)", ylab="Relative frequency", col="white", border=gray(0.8))

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
     ylab="Empirical h(y)", xlab="y",
     xlim=c(1, 6), ylim=c(0, 5))

dev.off()

# Exploring the marginal distribution -------------------------------------
library(gamlss)
library(RelDists)

# model is an R package hosted in github with an useful
# fuction called est_param() to compare models
remotes::install_github("fhernanb/model")
library(model)

# Choosing the distribution between Realplus
mod_gen <- gamlss(y~1, family=GA)

ch <- chooseDist(object=mod_gen, type="realplus", 
                 k=2, extra=c("FWE"))

ch

ch <- na.omit(ch)
ch

orden <- order(ch[, 1])
ch <- ch[orden, , drop = FALSE]
nombres_ordenados <- rownames(ch)
rownames(ch) <- nombres_ordenados
ch


# Fitting the model
library(RelDists)
library(gamlss)
mod <- gamlss(y~1, family='FWE')

summary(mod)

exp(coef(mod, what="mu"))
exp(coef(mod, what="sigma"))
AIC(mod)

# Next we fit the model using
# gamlss robust
source("https://raw.githubusercontent.com/fhernanb/dist_gamlss_book/main/%5B03%5D%20Robust%20estimation%20example/gamlssRobust.R")

mod_rob <- gamlssRobust(mod)

summary(mod_rob)

exp(coef(mod_rob, what="mu"))
exp(coef(mod_rob, what="sigma"))
AIC(mod_rob)

# With other distributions to compare the results

# Wei
mod_WEI <- gamlss(y~1, family='WEI')
summary(mod_WEI)
exp(coef(mod_WEI, what="mu"))
exp(coef(mod_WEI, what="sigma"))
AIC(mod_WEI)

mod_WEI_rob <- gamlssRobust(mod_WEI, trace=FALSE)
summary(mod_WEI_rob)
exp(coef(mod_WEI_rob, what="mu"))
exp(coef(mod_WEI_rob, what="sigma"))
AIC(mod_WEI_rob)

# Gamma
mod_GA <- gamlss(y~1, family='GA')
summary(mod_GA)
exp(coef(mod_GA, what="mu"))
exp(coef(mod_GA, what="sigma"))
AIC(mod_GA)

mod_GA_rob <- gamlssRobust(mod_GA, trace=FALSE)
summary(mod_GA_rob)
exp(coef(mod_GA_rob, what="mu"))
exp(coef(mod_GA_rob, what="sigma"))
AIC(mod_GA_rob)

# Applying Kolmogorov-Smirnov test
y[29] <- 2.93701 # Para evitar un empate

ks.test(x=y, "pFWE", mu=0.7512, sigma=8.3201)
ks.test(x=y, "pFWE", mu=0.8967, sigma=9.5267)

ks.test(x=y, "pWEI", mu=3.3147, sigma=5.0495)
ks.test(x=y, "pWEI", mu=3.2595, sigma=5.8541)

ks.test(x=y, "pGA", mu=3.0593, sigma=0.1977)
ks.test(x=y, "pGA", mu=3.0279, sigma=0.1859)

# The fitted values
mu_hat  <- exp(coef(mod_rob, what="mu"))
sig_hat <- exp(coef(mod_rob, what="sigma"))

mu_hat
sig_hat

# FIGURE
pdf('Figs/res_example_2.pdf', width=12, height=12)

par(mfrow=c(2, 2))

# f(y)
hist(y, freq=FALSE, las=1, main="(a)", xlim=c(1, 6), ylim=c(0, 0.7),
     ylab="f(y)", xlab="y", col="white", border=gray(0.8))
curve(dFWE(x, mu=mu_hat, sigma=sig_hat), from=0, to=7, ylim=c(0, 1),
      col="black", las=1, lwd=2, add=TRUE)

# F(y)
plot(ecdf(x=y), ylim=c(0, 1), main="(b)", las=1, ylab="F(y)", xlab="y", 
     col=gray(0.8), xaxt='n', xlim=c(1, 6))
axis(side=1, at=seq(from=1, to=6, by=1), labels=seq(from=1, to=6, by=1))

# S(y)
curve(pFWE(x, mu=mu_hat, sigma=sig_hat, lower.tail=TRUE, log.p=FALSE),
      from=0, to=25, col="black", lwd=2, add=TRUE, main="(c)")

fun <- ecdf(y)
sfun0  <- stepfun(x=sort(y), y=c(1, 1-fun(sort(y))), right=FALSE)
plot(sfun0, pch=19, verticals=FALSE, las=1, xlim=c(1, 6),
     main="(c)", ylab="R(y)", xlab="y", col=gray(0.8), xaxt='n')
axis(side=1, at=seq(from=1, to=6, by=1), labels=seq(from=1, to=6, by=1))
abline(h=c(0, 1), col="gray70", lty=2)
curve(pFWE(x, mu=mu_hat, sigma=sig_hat, lower.tail=FALSE, log.p=FALSE),
      from=0, to=25, col="black", lwd=2, las=1, ylab="R(y)", xlab="y",
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
     ylab="Empirical h(y)", xlab="y",
     xlim=c(1, 6), ylim=c(0, 5))

curve(hFWE(x, mu=mu_hat, sigma=sig_hat), from=0, to=5, #ylim=c(0, 2.5), 
      col="black", lwd=2, las=1, ylab="h(y)", xlab="y", add=TRUE)

dev.off()



