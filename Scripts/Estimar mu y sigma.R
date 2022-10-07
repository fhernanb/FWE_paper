# En este script se implementa la estimacion sencilla de mu y sigma
# planteada por Bebbington

# https://www.fpl.fs.fed.us/documnts/fplgtr/fpl_gtr264.pdf

# Datos de Bebbington
t <- c(2.160, 0.746, 0.402, 0.954, 0.491, 6.560, 4.992, 0.347,
       0.150, 0.358, 0.101, 1.359, 3.465, 1.060, 0.614, 1.921,
       4.082, 0.199, 0.605, 0.273, 0.070, 0.062, 5.320)

F_hat <- ecdf(t)
p <- F_hat(t)
p[p == 1] <- 0.999
y <- log(-log(1-p))
x1 <- t
x2 <- -1/t
mod <- lm(y ~ 0 + x1 + x2)
coef(mod)

estim_mu_sigma_FWE <- function(y) {
  F_hat <- ecdf(y)
  p <- F_hat(y)
  p[p == 1] <- 0.999
  yy <- log(-log(1-p))
  x1 <- y
  x2 <- -1/y
  mod <- lm(yy ~ 0 + x1 + x2)
  coef(mod)
}

estim_mu_sigma(t)[1]
estim_mu_sigma(t)[2]
