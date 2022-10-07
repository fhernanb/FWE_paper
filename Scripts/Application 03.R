
library(readxl)
data <- read_excel("Data/moret_tatay_et_al.2022.xlsx")

# retaining correct RTs that are between 250 and 1800 milliseconds
data2 <- subset(data, RT<1800 & RT>250 & Accuracy==1)

# ensuring all variables of interest are well
data2$Participant <- as.factor(data2$Participant)
data2$Group <- as.factor(data2$Group)
data2$Condition <- as.factor(data2$Condition)

library(gamlss)
library(RelDists)


# Exploring the data ------------------------------------------------------

y <- data2$RT

# FIGURA
#pdf('hist_ecdf_esurv.pdf', width=12, height=12)
par(mfrow=c(2, 2))
# Histogram
hist(y, freq=FALSE, main="(a)", ylab="Relative frequency", breaks=50,
     xlab="y", col="white", xaxt="n")
axis(side=1, at=seq(0, 2000, 500), labels=seq(0, 2000, 500))
# Empirical cdf
plot(ecdf(x=y), las=1,
     main="(b)", ylab="Empirical F(y)", xlab="y", col=gray(0.8))
# Empirical surv
fun <- ecdf(y)
sfun0  <- stepfun(x=sort(y), y=c(1, 1-fun(sort(y))), right=FALSE)
plot(sfun0, pch = 19, verticals = FALSE, las=1,
     main="(c)", ylab="Empirical R(y)", xlab="y", col=gray(0.8))
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
     ylab="Empirical H(y)", xlab="y")

#dev.off()


# Fitting the model -------------------------------------------------------

mod0 <- gamlss(RT ~ Group + Condition + random(Participant), 
               sigma.fo = ~ Group,
               family=FWE, data=data2,
               control=gamlss.control(n.cyc=1000),
               i.control=glim.control(bf.cyc=1000))

saveRDS(object=mod0, file="mod_fwe.RDS")
mod0 <- readRDS(file="mod_fwe.RDS")

summary(mod0)
wp(mod0)
plot(mod0)
Rsq(mod0)

getSmo(mod0)

mod1 <- gamlss(RT ~ Group + Condition + random(Participant), 
               sigma.fo = ~ Group,
               family=exGAUS, data=data2,
               control=gamlss.control(n.cyc=1000),
               i.control=glim.control(bf.cyc=1000))

saveRDS(object=mod1, file="mod_exgauss.RDS")
mod1 <- readRDS(file="mod_exgauss.RDS")

summary(mod1)
wp(mod1)
plot(mod1)
Rsq(mod1)

getSmo(mod1)


# Exploring the data ------------------------------------------------------

library(readxl)
data <- read_excel("moret_tatay_et_al.2022.xlsx")
data2 <- data

# retaining correct RTs that are between 250 and 1800 milliseconds
data2 <- data %>% filter(RT<1800 & RT>250)
#data2 <- data %>% filter(RT<1800 & RT>250 & Accuracy==1)

# ensuring all variables of interest are well
data2$Participant <- as.factor(data2$Participant)
data2$Group       <- as.factor(data2$Group)
data2$Condition   <- as.factor(data2$Condition)

library(dplyr)

data2 %>% group_by(Group, Condition) %>% 
  summarise(mean_rt = mean(RT),
            sd_rt = sd(RT),
            hits = mean(Accuracy))

d1 <- data2 %>% filter(Group=="4_yo", Condition=="control")

mod <- gamlss(RT ~ 1, family=exGAUS, data=d1)
mod
mean(d1$RT)
logLik(mod)
AIC(mod)


# Replicating the results from Moret-Tatay (2022) table 1 -----------------


