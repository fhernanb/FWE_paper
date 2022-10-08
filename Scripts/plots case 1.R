# Plots -------------------------------------------------------------------
dt <- read.table('Simulated_data/simul_without_cov.txt', 
                 col.names=c('mu', 'sigma', 'n', 'censura'))

require(dplyr)
library(tidyr)

# Numero de observaciones por combinacion de n y censura
num <- dt %>% group_by(censura, n) %>% count()
mean(num$nn)
min(num$nn)

# The parameters ----------------------------------------------------------
true_mu    <- 0.21
true_sigma <- 0.25

# Mean
res <- dt %>% 
  drop_na() %>% 
  group_by(censura, n) %>% 
  summarise(mean_mu=mean(mu), 
            mean_sigma=mean(sigma),
            bias_mu=true_mu - mean(mu), 
            bias_sigma=true_sigma - mean(sigma),
            mse_mu=mean((true_mu - mu)^2), 
            mse_sigma=mean((true_sigma - sigma)^2))

res$Censored <- as.factor(res$censura)

library(ggplot2)
library(gridExtra)

# Mean -----------------------------------------------------
p1 <- ggplot(data=res, aes(x=n, y=mean_mu, group=Censored)) + 
  geom_line(aes(color=Censored)) + 
  labs(x="n", y=expression(hat(mu))) +
  geom_line(y=true_mu, col='red', lty='dashed')

p2 <- ggplot(data=res, aes(x=n, y=mean_sigma, group=Censored)) + 
  geom_line(aes(color=Censored)) + 
  labs(x="n", y=expression(hat(sigma))) +
  geom_line(y=true_sigma, col='red', lty='dashed')

mean1 <- grid.arrange(p1, p2, nrow = 1)
mean1
ggsave(filename="Figs/mean1.pdf", 
       plot=mean1, 
       width=10, height=4)

# Bias -----------------------------------------------------
p1 <- ggplot(data=res, aes(x=n, y=bias_mu, group=Censored)) + 
  geom_line(aes(color=Censored)) + 
  labs(x="n", y=expression(Bias~hat(mu)))

p2 <- ggplot(data=res, aes(x=n, y=bias_sigma, group=Censored)) + 
  geom_line(aes(color=Censored)) + 
  labs(x="n", y=expression(Bias~hat(sigma)))

bias1 <- grid.arrange(p1, p2, nrow = 1)
bias1
ggsave(filename="Figs/bias1.pdf", 
       plot=bias1, 
       width=10, height=4)

# MSE -----------------------------------------------------
p1 <- ggplot(data=res, aes(x=n, y=mse_mu, group=Censored)) + 
  geom_line(aes(color=Censored)) + 
  labs(x="n", y=expression(MSE~hat(mu)))

p2 <- ggplot(data=res, aes(x=n, y=mse_sigma, group=Censored)) + 
  geom_line(aes(color=Censored)) + 
  labs(x="n", y=expression(MSE~hat(sigma)))

mse1 <- grid.arrange(p1, p2, nrow = 1)
mse1
ggsave(filename="Figs/mse1.pdf", 
       plot=mse1, 
       width=10, height=4)

