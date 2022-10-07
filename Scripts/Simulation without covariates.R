require(RelDists)
require(gamlss)

setwd("C:/Users/fhbapto/Dropbox/Investigacion/Reliability distributions/01 FWE paper/R code for paper")
setwd("G:/Mi unidad/13_Mis_Articulos/FWE distribution/R code for paper")

# The parameters ----------------------------------------------------------
true_mu    <- 0.21
true_sigma <- 0.25

# Useful functions to the simulation study --------------------------------

# Funcion para obtener mu_hat y sigma_hat para un valor fijo de n
simul_one <- function(size) {
  y <- rFWE(n=size, mu=true_mu, sigma=true_sigma)
  mod <- NULL
  mod <- gamlss(y~1, sigma.fo=~1, family='FWE',
                control=gamlss.control(n.cyc=2500, trace=FALSE))
  res <- c(exp(coef(mod, what='mu')), exp(coef(mod, what='sigma')))
  res
}

# Super function to simulate and write the estimated parameters
simul <- function(n) {
  result <- t(replicate(n=nrep, expr=simul_one(size=n)))
  result <- cbind(result, n)
  write(x=t(result), file='simul_without_cov.txt', 
        ncol=3, append=TRUE)
}


# Code to generate the simulations given n --------------------------------

# Aqui se definen los valores de tamano muestral n
# Luego se define el numero de repeticiones
n <- seq(from=20, to=300, by=10)
nrep <- 1000

values <- expand.grid(n=n)
values
apply(values, 1, simul)


# Plots -------------------------------------------------------------------
dt <- read.table('simul_without_cov.txt', 
                 col.names=c('mu', 'sigma', 'n'))

require(dplyr)

# Mean
res <- dt %>% group_by(n) %>% summarise(mu=mean(mu), 
                                        sigma=mean(sigma))

pdf('mean_simul1.pdf', width=8, height=4)
par(mfrow=c(1, 2))
with(res, plot(x=n, y=mu, 
               type='b', las=1, ylab='Mean value', main=expression(mu),
               ylim=c(0.18, 0.28)))
abline(h=true_mu, lty='dashed', col='red')
with(res, plot(x=n, y=sigma, 
               type='b', las=1, ylab='Mean value', main=expression(sigma),
               ylim=c(0.24, 0.29)))
abline(h=true_sigma, lty='dashed', col='red')
dev.off()

# Bias
res <- dt %>% group_by(n) %>% summarise(mu=true_mu - mean(mu), 
                                        sigma=true_sigma - mean(sigma))

pdf('bias_simul1.pdf', width=8, height=4)
par(mfrow=c(1, 2))
with(res, plot(x=n, y=mu, 
               type='b', las=1, ylab='Bias', main=expression(mu)))
abline(h=0, lty='dashed', col='red')
with(res, plot(x=n, y=sigma, 
               type='b', las=1, ylab='Bias', main=expression(sigma)))
abline(h=0, lty='dashed', col='red')
dev.off()

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


