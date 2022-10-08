# En este archivo voy a crear una funcion para dibujar la s(t) y la h(t)
# empirica a partir de una muestra aleatoria.
# Debo tener en cuenta lo siguiente:
# s(t) = 1 - F(t)
# h(t) = f(t) / s(t)


# Usando los datos del ejemplo
y <- c(2.160, 0.746, 0.402, 0.954, 0.491, 6.560, 4.992, 0.347,
       0.150, 0.358, 0.101, 1.359, 3.465, 1.060, 0.614, 1.921,
       4.082, 0.199, 0.605, 0.273, 0.070, 0.062, 5.320)

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
     las=1, col="gray70", type="s",
     ylab="Hazard", xlab="y",
     xlim=c(0, 5), ylim=c(0, 2.5))

library(RelDists)
curve(hFWE(x, mu = 0.2065115, sigma = 0.2588857), 
      from=0, to=5, ylim=c(0, 2.5), col="tomato", lwd=2, las=1, add=TRUE)


# Repeating the plot but using simulated data
n <- 100
mu <- 0.25
sigma <- 1.125

y <- rFWE(n=n, mu = mu, sigma = sigma)

emp_sur <- esf(y)
emp_pdf <- with(density(y), approxfun(x, y, rule=1))

plot(x=sort(y), y=emp_pdf(sort(y)) / emp_sur(sort(y)), 
     las=1, col="gray70", type="s",
     ylab="Hazard", xlab="y",
     xlim=c(0, 7), ylim=c(0, 2.5))

library(RelDists)
curve(hFWE(x, mu = mu, sigma = sigma), 
      from=0, to=7, ylim=c(0, 2.5), col="tomato", lwd=2, las=1, add=TRUE)

# Creando la empirical hazard con el paquete muhaz
library(muhaz)
cause0 <- rep(1, times=length(y))
haz <- muhaz(y, cause0)
plot(haz, xlim=c(0, 7), ylim=c(0, 2.5), las=1)

curve(hFWE(x, mu = mu, sigma = sigma), 
      from=0, to=7, ylim=c(0, 2.5), col="tomato", lwd=2, las=1, add=TRUE)

