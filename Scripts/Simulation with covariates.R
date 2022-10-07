require(RelDists)
require(gamlss)

setwd("C:/Users/fhbapto/Dropbox/Investigacion/Reliability distributions/01 FWE paper/R code for paper")
setwd("G:/Mi unidad/13_Mis_Articulos/FWE distribution/R code for paper")

# The parameters ----------------------------------------------------------
b0 <- -2
b1 <- 0.9
g0 <- 2
g1 <- -6.7

# Useful functions to the simulation study --------------------------------

# Funcion para obtener mu_hat y sigma_hat para un valor fijo de n
simul_one <- function(size) {
  x1 <- runif(n=size)
  x2 <- runif(n=size)
  mu <- exp(b0 + b1 * x1)
  sig <- exp(g0 + g1 * x2)
  y <- rFWE(n=size, mu=mu, sigma=sig)
  mod <- NULL
  mod <- try(gamlss(y~x1, sigma.fo=~x2, family='FWE',
                control=gamlss.control(n.cyc=2500, trace=FALSE)))
  if (class(mod)[1] == "try-error")
    res <- rep(NA, 4)
  else
    res <- c(coef(mod, what='mu'), coef(mod, what='sigma'))
  res
}

# Super function to simulate and write the estimated parameters
simul <- function(n) {
  result <- t(replicate(n=nrep, expr=simul_one(size=n)))
  result <- cbind(result, n)
  write(x=t(result), file='simul_with_cov.txt', 
        ncol=5, append=TRUE)
}


# Code to generate the simulations given n --------------------------------

# Aqui se definen los valores de tamano muestral n
# Luego se define el numero de repeticiones
n <- seq(from=10, to=300, by=20)
nrep <- 1000

values <- expand.grid(n=n)
values
apply(values, 1, simul)


# Plots -------------------------------------------------------------------
dt <- read.table('simul_with_cov.txt', 
                 col.names=c('b0', 'b1', 'g0', 'g1', 'n'))

require(dplyr)

# Mean
res <- dt %>% group_by(n) %>% summarise(b0=mean(b0, na.rm=TRUE),
                                        b1=mean(b1, na.rm=TRUE),
                                        g0=mean(g0, na.rm=TRUE),
                                        g1=mean(g1, na.rm=TRUE))

#pdf('mean_simul1.pdf', width=8, height=4)
par(mfrow=c(2, 2))
with(res, plot(x=n, y=b0, 
               type='b', las=1, ylab='Mean value', main=expression(beta[0])))
abline(h=b0, lty='dashed', col='red')
with(res, plot(x=n, y=b1, 
               type='b', las=1, ylab='Mean value', main=expression(beta[1])))
abline(h=b1, lty='dashed', col='red')
with(res, plot(x=n, y=g0, 
               type='b', las=1, ylab='Mean value', main=expression(gamma[0])))
abline(h=g0, lty='dashed', col='red')
with(res, plot(x=n, y=g1, 
               type='b', las=1, ylab='Mean value', main=expression(gamma[1])))
abline(h=g1, lty='dashed', col='red')
#dev.off()

# Bias
res <- dt %>% group_by(n) %>% summarise(mu=true_mu - mean(mu), 
                                        sigma=true_sigma - mean(sigma))

#pdf('bias_simul1.pdf', width=8, height=4)
par(mfrow=c(1, 2))
with(res, plot(x=n, y=mu, 
               type='b', las=1, ylab='Bias', main=expression(mu)))
abline(h=0, lty='dashed', col='red')
with(res, plot(x=n, y=sigma, 
               type='b', las=1, ylab='Bias', main=expression(sigma)))
abline(h=0, lty='dashed', col='red')
#dev.off()

# MSE
res <- dt %>% group_by(n) %>% summarise(mu=mean((true_mu - mu)^2), 
                                        sigma=mean((true_sigma - sigma)^2))

pdf('mse_simul1.pdf', width=8, height=4)
par(mfrow=c(1, 2))
with(res, plot(x=n, y=mu, 
               type='b', las=1, ylab='MSE', main=expression(mu)))
abline(h=0, lty='dashed', col='red')
with(res, plot(x=n, y=sigma, 
               type='b', las=1, ylab='MSE', main=expression(sigma)))
abline(h=0, lty='dashed', col='red')
dev.off()


