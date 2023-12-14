library(RelDists)
library(gamlss)
library(gamlss.cens)
library(survival)

# The parameters ----------------------------------------------------------
b0 <- -2
b1 <- 0.9
g0 <- 2
g1 <- -6.7

# Se genera la familia de funciones de FWE censurada a derecha llamada FWErc
gen.cens(family="FWE", type="right")

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

# Funcion para obtener mu_hat y sigma_hat para un valor fijo de n
simul_one <- function(size, censura) {
  x1 <- runif(n=size)
  x2 <- runif(n=size)
  mu <- exp(b0 + b1 * x1)
  sig <- exp(g0 + g1 * x2)
  y <- rFWE(n=size, mu=mu, sigma=sig)
  y <- censurando(y, censura)
  mod <- NULL
  mod <- try(gamlss(y~x1, sigma.fo=~x2, family=FWErc,
                control=gamlss.control(c.crit=0.0001,
                                       n.cyc=2000, 
                                       trace=FALSE)),
             silent=TRUE)
  if (class(mod)[1] == "try-error")
    res <- rep(NA, 4)
  else
    res <- c(coef(mod, what='mu'), coef(mod, what='sigma'))
  return(res)
}

# Super function to simulate and write the estimated parameters
simul <- function(x) {
  n <- x[1]
  censura <- x[2]
  result <- t(replicate(n=nrep, expr=simul_one(size=n, censura=censura)))
  result <- cbind(result, n, censura)
  write(x=t(result), file='simul_with_cov.txt', ncol=6, append=TRUE)
}

# Code to generate the simulations given n --------------------------------
# Aqui se definen los valores de tamano muestral n
# Aqui se definen los valores porcentajes de censura
# Luego se define el numero de repeticiones
#n <- seq(from=20, to=300, by=10)
#censura <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5)
#nrep <- 10

#values <- expand.grid(n=n, censura=censura)
#values
#apply(values, 1, simul)

n <- seq(from=20, to=300, by=20)
censura <- c(0, 0.1, 0.2, 0.3)
nrep <- 10000

values <- expand.grid(n=n, censura=censura)
values

apply(values, 1, simul)

