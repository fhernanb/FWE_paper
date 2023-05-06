# Script for the third example of the FWE paper

library(dplyr)
library(readxl)

data <- read_excel("Data/DIB_data_BRUHA.xlsx", 
                   sheet = "Data_in_Brief", na = "NaN")

View(data)
str(data)
dplyr::glimpse(data)

# vectors of interest

# reaction time via hands
data$Reaction_time_hands <- as.numeric(data$Reaction_time_hands)
# reaction time via legs
data$Reaction_time_legs <- as.numeric(data$Reaction_time_legs)
# heart rate (ECG)

# Converting to factor some qualitative variables
data$FOOD <- factor(data$FOOD)
data$SMOKING <- factor(data$SMOKING)
data$ALKOHOL <- factor(data$ALKOHOL)

# Converting to numeric other variables
data$Sex <- as.numeric(data$Sex) # 0=Male, 1=Female
data$Age <- as.numeric(data$Age)
data$Systolic_pressure <- as.numeric(data$Systolic_pressure)
data$Diastolic_pressure <- as.numeric(data$Diastolic_pressure)
data$Puls <- as.numeric(data$Puls)
data$HR <- as.numeric(data$HR)
data$Height <- as.numeric(data$Height)
data$Weight <- as.numeric(data$Weight)
data$BMI <- as.numeric(data$BMI)
data$Muscle_mass <- as.numeric(data$Muscle_mass)
data$Water <- as.numeric(data$Water)
data$Fat <- as.numeric(data$Fat)
data$Flexibility <- as.numeric(data$Flexibility)
data$Gluctose <- as.numeric(data$Gluctose)

data$DRINK <- as.numeric(data$DRINK)
data$SUPPLEMENTS <- as.numeric(data$SUPPLEMENTS)
data$DOCTOR <- as.numeric(data$DOCTOR)
data$PARTNER <- as.numeric(data$PARTNER)
data$REST <- as.numeric(data$REST)


# Avoiding NA's
data <- na.omit(data)

# Exploring data ----------------------------------------------------------
table(data$Group)
table(data$Sex)
barplot(table(data$Age))


# hands RT data -----------------------------------------------------------
# Here I filter for reaction times > 400
data <- data |> filter(Reaction_time_hands > 400)

plot(density(data$Reaction_time_hands, na.rm=TRUE), 
     ylab="Density", xlab="Reaction time hands (segs)",
     main="", lwd=3, las=1)

# Relation between X's and Y ----------------------------------------------
with(data, boxplot(Reaction_time_hands ~ Sex))
with(data, boxplot(Reaction_time_hands ~ DRINK))
with(data, boxplot(Reaction_time_hands ~ SUPPLEMENTS))
with(data, boxplot(Reaction_time_hands ~ DOCTOR))
with(data, boxplot(Reaction_time_hands ~ PARTNER))
with(data, boxplot(Reaction_time_hands ~ REST))
with(data, boxplot(Reaction_time_hands ~ SMOKING))
with(data, boxplot(Reaction_time_hands ~ ALKOHOL))
with(data, boxplot(Reaction_time_hands ~ FOOD))


with(data, plot(y=Reaction_time_hands, x=Age))
with(data, plot(y=Reaction_time_hands, x=Systolic_pressure))
with(data, plot(y=Reaction_time_hands, x=Diastolic_pressure))
with(data, plot(y=Reaction_time_hands, x=Puls))
with(data, plot(y=Reaction_time_hands, x=HR))
with(data, plot(y=Reaction_time_hands, x=Height))
with(data, plot(y=Reaction_time_hands, x=Weight))
with(data, plot(y=Reaction_time_hands, x=BMI))
with(data, plot(y=Reaction_time_hands, x=Muscle_mass))
with(data, plot(y=Reaction_time_hands, x=Water))
with(data, plot(y=Reaction_time_hands, x=Fat))
with(data, plot(y=Reaction_time_hands, x=Flexibility))
with(data, plot(y=Reaction_time_hands, x=Gluctose))

# Exploring the marginal distribution -------------------------------------
library(gamlss)
library(RelDists)
library(model)

histDist(y=data$Reaction_time_hands, family="FWE")
histDist(y=data$Reaction_time_hands, family="WEI")
histDist(y=data$Reaction_time_hands, family="GA")

# Explorando el uso de chooseDist
mod_gen <- gamlss(Reaction_time_hands ~ 1, data=data) 
chooseDist(mod_gen, k=2, 
           type="extra", extra=c("FWE", "WEI", "GA"))


# Solo con X's cuanti -----------------------------------------------------

full_mod <- formula( ~ Age + 
                       Systolic_pressure + 
                       Diastolic_pressure +
                       Puls + 
                       HR + 
                       Height + 
                       Weight + 
                       BMI +
                       Muscle_mass + 
                       Water + 
                       Fat + 
                       Flexibility +
                       Gluctose)

mod0 <- gamlss(Reaction_time_hands ~ 1,
               sigma.fo= ~ 1,
               family=FWE,
               data=data,
               control=gamlss.control(n.cyc=15000))

# Usando stepGAIC
mod1 <- stepGAIC(mod0, scope=list(lower=~1, upper=full_mod),
                 what="mu", direction="both")
summary(mod1)
Rsq(mod1)

mod2 <- stepGAIC(mod1, scope=list(lower=~1, upper=full_mod),
                 what="sigma", direction="both")
summary(mod2)
Rsq(mod2)

y_hat <- est_param(mod2, fun = "mean", m = 10000)
cor(y_hat, data$Reaction_time_hands)

# Salvando los modelos
saveRDS(object=mod2, file="Fitted_models/mod2_fwe_solo_cuali_bruha.RDS")
     mod2 <- readRDS(file="Fitted_models/mod2_fwe_solo_cuali_bruha.RDS")

# Solo con X's CUALI -----------------------------------------------------

full_mod <- formula( ~ Sex +
                       DRINK +
                       SUPPLEMENTS +
                       DOCTOR +
                       PARTNER +
                       REST +
                       SMOKING +
                       ALKOHOL +
                       FOOD)

mod0 <- gamlss(Reaction_time_hands ~ 1,
               sigma.fo= ~ 1,
               family=GA,
               data=data,
               control=gamlss.control(n.cyc=15000))

# Usando stepGAIC
mod1 <- stepGAIC(mod0, scope=list(lower=~1, upper=full_mod),
                 what="mu", direction="both")
summary(mod1)
Rsq(mod1)

mod2 <- stepGAIC(mod1, scope=list(lower=~1, upper=full_mod),
                 what="sigma", direction="both")
summary(mod2)
Rsq(mod2)

y_hat <- est_param(mod2, fun = "mean", m = 10000)
cor(y_hat, data$Reaction_time_hands)


# Con cuanti y CUALI ------------------------------------------------------

# Original variables
mod0 <- gamlss(Reaction_time_hands ~ 1,
               sigma.fo= ~ 1,
               family=WEI,
               data=data,
               control=gamlss.control(n.cyc=15000))

full_mod <- formula( ~ Age + 
                       Systolic_pressure + 
                       Diastolic_pressure +
                       Puls + 
                       HR + 
                       Height + 
                       Weight + 
                       BMI +
                       Muscle_mass + 
                       Water + 
                       Fat + 
                       Flexibility +
                       Gluctose +
                       
                       Sex +
                       DRINK +
                       SUPPLEMENTS +
                       DOCTOR +
                       PARTNER +
                       REST +
                       SMOKING +
                       ALKOHOL +
                       FOOD)

# Usando stepGAIC
mod1 <- stepGAIC(mod0, scope=list(lower=~1, upper=full_mod),
                 what="mu", direction="both")
summary(mod1)
Rsq(mod1)

mod2 <- stepGAIC(mod1, scope=list(lower=~1, upper=full_mod),
                 what="sigma", direction="both")
summary(mod2)
Rsq(mod2)

y_hat <- est_param(mod2, fun = "mean", m = 10000)
cor(y_hat, data$Reaction_time_hands)


# Explorando el mejor modelo ----------------------------------------------
library(gamlss)
library(RelDists)
library(model)

mod_fwe <- gamlss(Reaction_time_hands ~ Age + Height + Water + HR,
                  sigma.fo= ~ Height + Puls + BMI + HR + Weight + Muscle_mass + Water,
                  family=FWE,
                  data=data,
                  control=gamlss.control(n.cyc=15000))

mod_wei <- gamlss(Reaction_time_hands ~ Age + Water + Fat + Sex + HR + Puls,
                  sigma.fo= ~ HR + Systolic_pressure,
                  family=WEI,
                  data=data,
                  control=gamlss.control(n.cyc=15000))

mod_ga <- gamlss(Reaction_time_hands ~ Age + Height,
                 sigma.fo= ~ Systolic_pressure + Puls + Age + Diastolic_pressure + Flexibility,
                 family=GA,
                 data=data,
                 control=gamlss.control(n.cyc=15000))

# Chequeando los valores de las metricas
Rsq(mod_fwe)
y_hat <- est_param(mod_fwe, fun = "mean", m = 10000)
cor(y_hat, data$Reaction_time_hands)

Rsq(mod_wei)
y_hat <- est_param(mod_wei, fun = "mean", m = 10000)
cor(y_hat, data$Reaction_time_hands)

Rsq(mod_ga)
y_hat <- est_param(mod_ga, fun = "mean", m = 10000)
cor(y_hat, data$Reaction_time_hands)


pdf('Figs/bruha_data.pdf', width=9, height=11)
par(mfrow=c(3, 2))
with(data, hist(Reaction_time_hands, freq=FALSE, col="white", 
                main="(a)", xlab="Reaction time hands [ms]"))

with(data, plot(y=Reaction_time_hands, x=Age, las=1,
                main="(b)",
                ylab="Reaction time hands", xlab="Age [years]"))
fit <- lowess(x=data$Age, y=data$Reaction_time_hands, f=2/3)
lines(fit, lwd=1, col='black')

with(data, plot(y=Reaction_time_hands, x=Muscle_mass, las=1,
                main="(c)",
                ylab="Reaction time hands", xlab="Muscle mass [%]"))
fit <- lowess(x=data$Muscle_mass, y=data$Reaction_time_hands, f=2/3)
lines(fit, lwd=1, col='black')

with(data, plot(y=Reaction_time_hands, x=Height, las=1,
                main="(d)",
                ylab="Reaction time hands", xlab="Height [cm]"))
fit <- lowess(x=data$Height, y=data$Reaction_time_hands, f=2/3)
lines(fit, lwd=1, col='black')

with(data, plot(y=Reaction_time_hands, x=HR, las=1,
                main="(e)",
                ylab="Reaction time hands", xlab="HR"))
fit <- lowess(x=data$HR, y=data$Reaction_time_hands, f=2/3)
lines(fit, lwd=1, col='black')

with(data, plot(y=Reaction_time_hands, x=Water, las=1,
                main="(f)",
                ylab="Reaction time hands", xlab="Water"))
fit <- lowess(x=data$Water, y=data$Reaction_time_hands, f=2/3)
lines(fit, lwd=1, col='black')

dev.off()


# Residual analysis
library(car)

# Short names for residual object
res_fwe <- resid(mod_fwe)
names(res_fwe) <- 1:35 # To change the names
res_wei <- resid(mod_wei)
res_ga  <- resid(mod_ga)

# To plot the residual plot

pdf('Figs/bruha_residuals_models.pdf', width=9, height=7)
par(mfrow=c(2, 3))
qqPlot(res_fwe, dist="norm", mean=0, sd=1,
       main='Residuals for FWE model', col.lines="gray",
       ylab='Randomized quantile residuals', las=1)
qqPlot(res_wei, dist="norm", mean=0, sd=1, 
       main='Residuals for WEI model', col.lines="gray",
       ylab='Randomized quantile residuals', las=1)
qqPlot(res_ga,  dist="norm", mean=0, sd=1, 
       main='Residuals for GA model', col.lines="gray",
       ylab='Randomized quantile residuals', las=1)

my_wp(mod_fwe, main='Wormplot for FWE model', col="black")
my_wp(mod_wei, main='Wormplot for WEI model', col="black")
my_wp(mod_ga,  main='Wormplot for GA model', col="black")

dev.off()

# Testing if residuals follow normal distribution
ks.test(res_fwe, "pnorm", mean=0, sd=1)
ks.test(res_wei, "pnorm", mean=0, sd=1)
ks.test(res_ga, "pnorm", mean=0, sd=1)

library(stargazer)
stargazer(mod_fwe)

