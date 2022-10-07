require(RelDists)


# Figures with basic colors -----------------------------------------------

pdf('Figs/pdf_cdf.pdf', width=9, height=4)
par(mfrow=c(1, 2))
curve(dFWE(x, mu=0.75, sigma=0.5), from=0, to=3, ylim=c(0, 1.5), 
      col="red", ylab="f(x)", las=1)
curve(dFWE(x, mu=2.00, sigma=3.0), col="blue", add=TRUE)
curve(dFWE(x, mu=0.75, sigma=1.3), col="darkgreen", add=TRUE)
cap1 <- expression(paste(mu, "=", 0.75, ", ", sigma, "=", 0.5))
cap2 <- expression(paste(mu, "=", 2, ", ", sigma, "=", 3))
cap3 <- expression(paste(mu, "=", 0.75, ", ", sigma, "=", 1.3))
legend('topright', legend=c(cap1, cap2, cap3), 
       col=c("red", "blue", "darkgreen"), lty=c(1, 1, 1), bty="n")

curve(pFWE(x, mu=0.75, sigma=0.5), from=0, to=3, col="red",
      ylab="F(x)", las=1)
curve(pFWE(x, mu=2, sigma=3), col="blue", add=TRUE)
curve(pFWE(x, mu=0.75, sigma=1.3), col="darkgreen", add=TRUE)
cap1 <- expression(paste(mu, "=", 0.75, ", ", sigma, "=", 0.5))
cap2 <- expression(paste(mu, "=", 2.00, ", ", sigma, "=", 3.0))
cap3 <- expression(paste(mu, "=", 0.75, ", ", sigma, "=", 1.3))
legend('bottomright', legend=c(cap1, cap2, cap3), 
       col=c("red", "blue", "darkgreen"), lty=c(1, 1, 1), bty="n")
dev.off()


pdf('hazard.pdf', width=6, height=5)
curve(hFWE(x, mu=0.75, sigma=0.5), from=0, to=3, ylim=c(0, 3), 
      col="red", ylab="h(x)", las=1)
curve(hFWE(x, mu=2, sigma=3), col="blue", add=T)
curve(hFWE(x, mu=0.75, sigma=1.3), col="green", add=T)
cap1 <- expression(paste(mu, "=", 0.75, ", ", sigma, "=", 0.5))
cap2 <- expression(paste(mu, "=", 2.00, ", ", sigma, "=", 3.0))
cap3 <- expression(paste(mu, "=", 0.75, ", ", sigma, "=", 1.3))
legend('bottomright', legend=c(cap1, cap2, cap3), 
       col=c("red", "blue", "green"), lty=c(1, 1, 1), bty="n")
dev.off()



# Using pallete for colors ------------------------------------------------

require(gplots)
paleta <- palette(rich.colors(4))

# pdf
pdf('Figs/shapes_pdf_cdf.pdf', width=12, height=6)
par(mfrow=c(1, 2))

# bathtub-like shape
m <- 1/8
s1 <- 3/8
s2 <- 9/8
s3 <- 27/8
s4 <- 81/8

curve(dFWE(x, mu=m, sigma=s1), from=0, to=15, ylim=c(0, 1), 
      col=paleta[1], ylab="f(y)", xlab="y", las=1, lwd=2)
curve(dFWE(x, mu=m, sigma=s2), col=paleta[2], add=T, lwd=2)
curve(dFWE(x, mu=m, sigma=s3), col=paleta[3], add=T, lwd=2)
curve(dFWE(x, mu=m, sigma=s4), col=paleta[4], add=T, lwd=2)

cap1 <- as.expression(bquote(mu * ' = ' * .(m) * ', ' * sigma * ' = ' * .(s1)))
cap2 <- as.expression(bquote(mu * ' = ' * .(m) * ', ' * sigma * ' = ' * .(s2)))
cap3 <- as.expression(bquote(mu * ' = ' * .(m) * ', ' * sigma * ' = ' * .(s3)))
cap4 <- as.expression(bquote(mu * ' = ' * .(m) * ', ' * sigma * ' = ' * .(s4)))

legend('topright', legend=c(cap1, cap2, cap3, cap4), 
       col=paleta, lty=c(1, 1, 1), bty="n", lwd=2)

# shallower bathtup-like shape
s <- 9/8
m1 <- 1/8
m2 <- 2/8
m3 <- 3/8
m4 <- 4/8

curve(pFWE(x, mu=m1, sigma=s), from=0, to=10, ylim=c(0, 1), 
      col=paleta[1], ylab="F(y)", xlab="y", las=1, lwd=2)
curve(pFWE(x, mu=m2, sigma=s), col=paleta[2], add=T, lwd=2)
curve(pFWE(x, mu=m3, sigma=s), col=paleta[3], add=T, lwd=2)
curve(pFWE(x, mu=m4, sigma=s), col=paleta[4], add=T, lwd=2)

cap1 <- as.expression(bquote(mu * ' = ' * .(m1) * ', ' * sigma * ' = ' * .(s)))
cap2 <- as.expression(bquote(mu * ' = ' * .(m2) * ', ' * sigma * ' = ' * .(s)))
cap3 <- as.expression(bquote(mu * ' = ' * .(m3) * ', ' * sigma * ' = ' * .(s)))
cap4 <- as.expression(bquote(mu * ' = ' * .(m4) * ', ' * sigma * ' = ' * .(s)))

legend('bottomright', legend=c(cap1, cap2, cap3, cap4), 
       col=paleta, lty=c(1, 1, 1), bty="n", lwd=2)
dev.off()

# Hazard
pdf('Figs/shapes_hazard.pdf', width=12, height=6)
par(mfrow=c(1, 2))

# bathtub-like shape
m <- 1/8
s1 <- 3/8
s2 <- 9/8
s3 <- 27/8
s4 <- 81/8

curve(hFWE(x, mu=m, sigma=s1), from=0, to=15, ylim=c(0, 2), 
      col=paleta[1], ylab="h(y)", xlab="y", las=1, lwd=2)
curve(hFWE(x, mu=m, sigma=s2), col=paleta[2], add=T, lwd=2)
curve(hFWE(x, mu=m, sigma=s3), col=paleta[3], add=T, lwd=2)
curve(hFWE(x, mu=m, sigma=s4), col=paleta[4], add=T, lwd=2)

cap1 <- as.expression(bquote(mu * ' = ' * .(m) * ', ' * sigma * ' = ' * .(s1)))
cap2 <- as.expression(bquote(mu * ' = ' * .(m) * ', ' * sigma * ' = ' * .(s2)))
cap3 <- as.expression(bquote(mu * ' = ' * .(m) * ', ' * sigma * ' = ' * .(s3)))
cap4 <- as.expression(bquote(mu * ' = ' * .(m) * ', ' * sigma * ' = ' * .(s4)))

legend('topright', legend=c(cap1, cap2, cap3, cap4), 
       col=paleta, lty=c(1, 1, 1), bty="n", lwd=2)

# shallower bathtup-like shape
s <- 9/8
m1 <- 1/8
m2 <- 2/8
m3 <- 3/8
m4 <- 4/8

curve(hFWE(x, mu=m1, sigma=s), from=0, to=10, ylim=c(0, 2), 
      col=paleta[1], ylab="h(y)", xlab="y", las=1, lwd=2)
curve(hFWE(x, mu=m2, sigma=s), col=paleta[2], add=T, lwd=2)
curve(hFWE(x, mu=m3, sigma=s), col=paleta[3], add=T, lwd=2)
curve(hFWE(x, mu=m4, sigma=s), col=paleta[4], add=T, lwd=2)

cap1 <- as.expression(bquote(mu * ' = ' * .(m1) * ', ' * sigma * ' = ' * .(s)))
cap2 <- as.expression(bquote(mu * ' = ' * .(m2) * ', ' * sigma * ' = ' * .(s)))
cap3 <- as.expression(bquote(mu * ' = ' * .(m3) * ', ' * sigma * ' = ' * .(s)))
cap4 <- as.expression(bquote(mu * ' = ' * .(m4) * ', ' * sigma * ' = ' * .(s)))

legend('topright', legend=c(cap1, cap2, cap3, cap4), 
       col=paleta, lty=c(1, 1, 1), bty="n", lwd=2)
dev.off()


# Figures using ggplot2 ---------------------------------------------------

library(ggplot2)
library(tidyverse)

# Para crear la figura de pdf's

# Los valores de mu y sigma
m <- 1/8
s1 <- 3/8
s2 <- 9/8
s3 <- 27/8
s4 <- 81/8

# Para crear el tibble con los valores de mu y sigma
coefs <- tibble(mu=m, sigma=c(s1, s2, s3, s4))

# Para crear las leyendas
cap1 <- as.expression(bquote(mu * ' = ' * .(m) * ', ' * sigma * ' = ' * .(s1)))
cap2 <- as.expression(bquote(mu * ' = ' * .(m) * ', ' * sigma * ' = ' * .(s2)))
cap3 <- as.expression(bquote(mu * ' = ' * .(m) * ', ' * sigma * ' = ' * .(s3)))
cap4 <- as.expression(bquote(mu * ' = ' * .(m) * ', ' * sigma * ' = ' * .(s4)))

coefs %>% 
   mutate(curve = letters[row_number()]) %>%    # add curve name
   crossing(x = seq(0.1, 15, 0.1)) %>%    # repeat each row for every occurence of x
   mutate(y = dFWE(x, mu, sigma)) %>%    # compute y values
   ggplot(aes(x, y, color = curve)) + 
   geom_line() + 
   theme(legend.title=element_blank(),
         legend.justification=c(1, 1), 
         legend.position=c(1, 1)) -> p1

p1
