library(RelDists)
library(gamlss)
library(gamlss.cens)
library(survival)

setwd("G:/Mi unidad/13_Mis_Articulos/FWE distribution/R code for paper")

# The parameters ----------------------------------------------------------
true_mu    <- 0.21
true_sigma <- 0.25

# Useful functions to the simulation study --------------------------------

# Function to censor a vector with a censoring value
censurando <- function(y, censura) {
  if (censura > 0) {
    corte <- quantile(y, probs=1-censura)
    ind <- y > corte
    y[ind] <- corte
    status <- rep(1, times=length(y))
    status[ind] <- 0
    x <- Surv(y, status)
  }
  else {
    x <- Surv(y, rep(1, length(y)))
  }
  return(x)
}

# Funcion para obtener mu_hat y sigma_hat dado n y valor de censura
simul_one <- function(size, censura) {
  y <- rFWE(n=size, mu=true_mu, sigma=true_sigma)
  y <- censurando(y, censura)
  mod <- NULL
  mod <- try(gamlss(y~1, sigma.fo=~1, family=cens(FWE),
                control=gamlss.control(n.cyc=2500, trace=FALSE)),
             silent=TRUE)
  if (class(mod)[1] == 'gamlss')
    res <- c(exp(coef(mod, what='mu')), exp(coef(mod, what='sigma')))
  else 
    res <- c(NA, NA)
  return(res)
}

# Super function to simulate and write the estimated parameters
simul <- function(x) {
  n <- x[1]
  censura <- x[2]
  result <- t(replicate(n=nrep, expr=simul_one(size=n, censura=censura)))
  result <- cbind(result, n, censura)
  write(x=t(result), file='simul_without_cov.txt', ncol=4, append=TRUE)
}

# Code to generate the simulations given n --------------------------------
# Aqui se definen los valores de tamano muestral n
# Aqui se definen los valores porcentajes de censura
# Luego se define el numero de repeticiones
n <- seq(from=20, to=300, by=20)
censura <- c(0, 0.1, 0.2, 0.3)
nrep <- 300

values <- expand.grid(n=n, censura=censura)
values

apply(values, 1, simul)
apply(values, 1, simul)
apply(values, 1, simul)
apply(values, 1, simul)
apply(values, 1, simul)

apply(values, 1, simul)
apply(values, 1, simul)
apply(values, 1, simul)
apply(values, 1, simul)
apply(values, 1, simul)
