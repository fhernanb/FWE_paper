setwd("G:/Mi unidad/13_Mis_Articulos/FWE distribution/R code for paper")

# Plots -------------------------------------------------------------------
dt <- read.table('simul_with_cov.txt', 
                 col.names=c('b0', 'b1', 'g0', 'g1', 'n', 'censura'))

require(dplyr)
library(tidyr)

# Filtrando para quedar con las estimaciones coherentes
#dt <- dt %>% filter(b0 > -10 & b1 > -10)

# Numero de observaciones por combinacion de n y censura
num <- dt %>% group_by(censura, n) %>% count()
mean(num$nn)
min(num$nn)

# The parameters ----------------------------------------------------------
true_b0 <- -2
true_b1 <- 0.9
true_g0 <- 2
true_g1 <- -6.7

# Mean
res <- dt %>% 
  drop_na() %>% 
  group_by(censura, n) %>% 
  summarise(mean_b0=mean(b0),
            mean_b1=mean(b1),
            mean_g0=mean(g0),
            mean_g1=mean(g1),
            mse_b0=mean((true_b0 - b0)^2), 
            mse_b1=mean((true_b1 - b1)^2),
            mse_g0=mean((true_g0 - g0)^2), 
            mse_g1=mean((true_g1 - g1)^2))

res$Censored <- as.factor(res$censura)

library(ggplot2)
library(gridExtra)

# Mean -----------------------------------------------------
p1 <- ggplot(data=res, aes(x=n, y=mean_b0, group=Censored)) + 
  geom_line(aes(color=Censored)) + 
  labs(x="n", y=expression(hat(beta)[0])) +
  #theme_bw() +
  geom_line(y=true_b0, col='red', lty='dashed')

p2 <- ggplot(data=res, aes(x=n, y=mean_b1, group=Censored)) + 
  geom_line(aes(color=Censored)) + 
  labs(x="n", y=expression(hat(beta)[1])) +
  #theme_bw() +
  geom_line(y=true_b1, col='red', lty='dashed')

p3 <- ggplot(data=res, aes(x=n, y=mean_g0, group=Censored)) + 
  geom_line(aes(color=Censored)) + 
  labs(x="n", y=expression(hat(gamma)[0])) +
  #theme_bw() +
  geom_line(y=true_g0, col='red', lty='dashed')

p4 <- ggplot(data=res, aes(x=n, y=mean_g1, group=Censored)) + 
  geom_line(aes(color=Censored)) + 
  labs(x="n", y=expression(hat(gamma)[1])) +
  #theme_bw() +
  geom_line(y=true_g1, col='red', lty='dashed')

mean2 <- grid.arrange(p1, p2, p3, p4, nrow=2, ncol=2)
mean2
ggsave(filename="mean2.pdf", 
       plot=mean2, 
       width=10, height=8)

# MSE -----------------------------------------------------
p1 <- ggplot(data=res, aes(x=n, y=mse_b0, group=Censored)) + 
  geom_line(aes(color=Censored)) + 
  #theme_bw() +
  labs(x="n", y=expression(MSE~hat(beta)[0]))

p2 <- ggplot(data=res, aes(x=n, y=mse_b1, group=Censored)) + 
  geom_line(aes(color=Censored)) + 
  #theme_bw() +
  labs(x="n", y=expression(MSE~hat(beta)[1]))

p3 <- ggplot(data=res, aes(x=n, y=mse_g0, group=Censored)) + 
  geom_line(aes(color=Censored)) + 
  #theme_bw() +
  labs(x="n", y=expression(MSE~hat(gamma)[0]))

p4 <- ggplot(data=res, aes(x=n, y=mse_g1, group=Censored)) + 
  geom_line(aes(color=Censored)) + 
  #theme_bw() +
  labs(x="n", y=expression(MSE~hat(gamma)[1]))

mse2 <- grid.arrange(p1, p2, p3, p4, nrow=2, ncol=2)
mse2
ggsave(filename="mse2.pdf", 
       plot=mse2, 
       width=10, height=8)


