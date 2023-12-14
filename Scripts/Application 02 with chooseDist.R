# Script for the second example of the FWE paper

library(dplyr)
library(readxl)

datos <- read_excel("Data/DIB_data_BRUHA.xlsx", 
                    sheet = "Data_in_Brief", na = "NaN")

View(datos)
str(datos)
dplyr::glimpse(datos)

# vectors of interest

# reaction time via hands
datos$Reaction_time_hands <- as.numeric(datos$Reaction_time_hands)
# reaction time via legs
datos$Reaction_time_legs <- as.numeric(datos$Reaction_time_legs)
# heart rate (ECG)

# Converting to factor some qualitative variables
datos$FOOD <- factor(datos$FOOD)
datos$SMOKING <- factor(datos$SMOKING)
datos$ALKOHOL <- factor(datos$ALKOHOL)

# Converting to numeric other variables
datos$Sex <- as.numeric(datos$Sex) # 0=Male, 1=Female
datos$Age <- as.numeric(datos$Age)
datos$Systolic_pressure <- as.numeric(datos$Systolic_pressure)
datos$Diastolic_pressure <- as.numeric(datos$Diastolic_pressure)
datos$Puls <- as.numeric(datos$Puls)
datos$HR <- as.numeric(datos$HR)
datos$Height <- as.numeric(datos$Height)
datos$Weight <- as.numeric(datos$Weight)
datos$BMI <- as.numeric(datos$BMI)
datos$Muscle_mass <- as.numeric(datos$Muscle_mass)
datos$Water <- as.numeric(datos$Water)
datos$Fat <- as.numeric(datos$Fat)
datos$Flexibility <- as.numeric(datos$Flexibility)
datos$Gluctose <- as.numeric(datos$Gluctose)

datos$DRINK <- as.numeric(datos$DRINK)
datos$SUPPLEMENTS <- as.numeric(datos$SUPPLEMENTS)
datos$DOCTOR <- as.numeric(datos$DOCTOR)
datos$PARTNER <- as.numeric(datos$PARTNER)
datos$REST <- as.numeric(datos$REST)


# Avoiding NA's
datos <- na.omit(datos)

# Exploring data ----------------------------------------------------------
table(datos$Group)
table(datos$Sex)
barplot(table(datos$Age))


# hands RT data -----------------------------------------------------------
# Here I filter for reaction times > 400
datos <- datos |> filter(Reaction_time_hands > 400)

plot(density(datos$Reaction_time_hands, na.rm=TRUE), 
     ylab="Density", xlab="Reaction time hands (segs)",
     main="", lwd=3, las=1)

# Relation between X's and Y ----------------------------------------------
with(datos, boxplot(Reaction_time_hands ~ Sex))
with(datos, boxplot(Reaction_time_hands ~ DRINK))
with(datos, boxplot(Reaction_time_hands ~ SUPPLEMENTS))
with(datos, boxplot(Reaction_time_hands ~ DOCTOR))
with(datos, boxplot(Reaction_time_hands ~ PARTNER))
with(datos, boxplot(Reaction_time_hands ~ REST))
with(datos, boxplot(Reaction_time_hands ~ SMOKING))
with(datos, boxplot(Reaction_time_hands ~ ALKOHOL))
with(datos, boxplot(Reaction_time_hands ~ FOOD))


with(datos, plot(y=Reaction_time_hands, x=Age))
with(datos, plot(y=Reaction_time_hands, x=Systolic_pressure))
with(datos, plot(y=Reaction_time_hands, x=Diastolic_pressure))
with(datos, plot(y=Reaction_time_hands, x=Puls))
with(datos, plot(y=Reaction_time_hands, x=HR))
with(datos, plot(y=Reaction_time_hands, x=Height))
with(datos, plot(y=Reaction_time_hands, x=Weight))
with(datos, plot(y=Reaction_time_hands, x=BMI))
with(datos, plot(y=Reaction_time_hands, x=Muscle_mass))
with(datos, plot(y=Reaction_time_hands, x=Water))
with(datos, plot(y=Reaction_time_hands, x=Fat))
with(datos, plot(y=Reaction_time_hands, x=Flexibility))
with(datos, plot(y=Reaction_time_hands, x=Gluctose))

# Exploring the marginal distribution -------------------------------------
library(gamlss)
library(RelDists)

# model is an R package hosted in github with an useful
# fuction called est_param() to compare models

if (!require('devtools')) install.packages('devtools')
devtools::install_github('fhernanb/model', force=TRUE)
library(model)

# Choosing the distribution between Realplus
mod_gen <- gamlss(Reaction_time_hands~1, family=GA, data=datos)

ch <- chooseDist(object=mod_gen, type="realplus", 
                 k=2, extra=c("FWE"))

ch

ch <- na.omit(ch)
ch

orden <- order(ch[, 1])
ch <- ch[orden, , drop = FALSE]
nombres_ordenados <- rownames(ch)
rownames(ch) <- nombres_ordenados
ch


# Formula -----------------------------------------------------------------

full_mod <- formula( ~ 
                       Age +
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
                       Sex
                     )

# Model with GA -----------------------------------------------------------
mod_GA <- gamlss(Reaction_time_hands ~ 1,
                 sigma.fo= ~ 1,
                 family=GA,
                 data=datos,
                 control=gamlss.control(n.cyc=15000))

# Usando stepGAIC
mod_GA_1 <- stepGAIC(mod_GA, scope=list(lower=~1, upper=full_mod),
                     what="mu", direction="both")
summary(mod_GA_1)
Rsq(mod_GA_1)

mod_GA_2 <- stepGAIC(mod_GA_1, scope=list(lower=~1, upper=full_mod),
                 what="sigma", direction="both")
summary(mod_GA_2)
Rsq(mod_GA_2)

y_hat <- est_param(mod_GA_2, fun = "mean", m = 10000)
cor(y_hat, datos$Reaction_time_hands)

AIC(mod_GA_2)

# Model with IG -----------------------------------------------------------
mod_IG <- gamlss(Reaction_time_hands ~ 1,
                 sigma.fo= ~ 1,
                 family=IG,
                 data=datos,
                 control=gamlss.control(n.cyc=15000))

# Usando stepGAIC
mod_IG_1 <- stepGAIC(mod_IG, scope=list(lower=~1, upper=full_mod),
                     what="mu", direction="both")
summary(mod_IG_1)
Rsq(mod_IG_1)

mod_IG_2 <- stepGAIC(mod_IG_1, scope=list(lower=~1, upper=full_mod),
                     what="sigma", direction="both")
summary(mod_IG_2)
Rsq(mod_IG_2)

y_hat <- est_param(mod_IG_2, fun = "mean", m = 100)
cor(y_hat, datos$Reaction_time_hands)

AIC(mod_IG_2)

# Model with LOGNO -----------------------------------------------------------
mod_LOGNO <- gamlss(Reaction_time_hands ~ 1,
                 sigma.fo= ~ 1,
                 family=LOGNO,
                 data=datos,
                 control=gamlss.control(n.cyc=15000))

# Usando stepGAIC
mod_LOGNO_1 <- stepGAIC(mod_LOGNO, scope=list(lower=~1, upper=full_mod),
                     what="mu", direction="both")
summary(mod_LOGNO_1)
Rsq(mod_LOGNO_1)

mod_LOGNO_2 <- stepGAIC(mod_LOGNO_1, scope=list(lower=~1, upper=full_mod),
                     what="sigma", direction="both")
summary(mod_LOGNO_2)
Rsq(mod_LOGNO_2)

y_hat <- est_param(mod_LOGNO_2, fun = "mean", m = 10000)
cor(y_hat, datos$Reaction_time_hands)

AIC(mod_LOGNO_2)

# Model with FWE -----------------------------------------------------------
mod_FWE <- gamlss(Reaction_time_hands ~ 1,
                 sigma.fo= ~ 1,
                 family=FWE,
                 data=datos,
                 control=gamlss.control(n.cyc=15000))

# Usando stepGAIC
mod_FWE_1 <- stepGAIC(mod_FWE, scope=list(lower=~1, upper=full_mod),
                     what="mu", direction="both")
summary(mod_FWE_1)
Rsq(mod_FWE_1)

mod_FWE_2 <- stepGAIC(mod_FWE_1, scope=list(lower=~1, upper=full_mod),
                     what="sigma", direction="both")
summary(mod_FWE_2)
Rsq(mod_FWE_2)

y_hat <- est_param(mod_FWE_2, fun = "mean", m = 10000)
cor(y_hat, datos$Reaction_time_hands)

AIC(mod_FWE_2)

# Removing manually no significative variables for FWE model

mod_FWE_3 <- gamlss(Reaction_time_hands ~ Age + Height + Water + HR,
                    sigma.fo=~Height+Puls+BMI+HR+Weight+Muscle_mass+
                      Water,
                    family=FWE,
                    data=datos,
                    control=gamlss.control(n.cyc=15000))
summary(mod_FWE_3)

Rsq(mod_FWE_3)
y_hat <- est_param(mod_FWE_3, fun = "mean", m = 10000)
cor(y_hat, datos$Reaction_time_hands)
AIC(mod_FWE_3)


# Residual analysis -------------------------------------------------------
library(car)

# Short names for residual object
res_ga <- resid(mod_GA_2)
res_ig <- resid(mod_IG_2)
res_ln  <- resid(mod_LOGNO_2)
res_fwe  <- resid(mod_FWE_3)

# To change the names
names(res_ga) <- 1:35
names(res_ig) <- 1:35
names(res_ln) <- 1:35
names(res_fwe) <- 1:35

# To plot the residual plot

pdf('Figs/bruha_residuals_models.pdf', width=7, height=7)
par(mfrow=c(2, 2))
qqPlot(res_ga, dist="norm", mean=0, sd=1,
       main='Residuals for GA model', col.lines="gray",
       ylab='Randomized quantile residuals', las=1)
qqPlot(res_ig, dist="norm", mean=0, sd=1, 
       main='Residuals for IG model', col.lines="gray",
       ylab='Randomized quantile residuals', las=1)
qqPlot(res_ln,  dist="norm", mean=0, sd=1, 
       main='Residuals for LOGNO model', col.lines="gray",
       ylab='Randomized quantile residuals', las=1)
qqPlot(res_fwe,  dist="norm", mean=0, sd=1, 
       main='Residuals for FWE model', col.lines="gray",
       ylab='Randomized quantile residuals', las=1)
dev.off()

# We need a modification of the wp of gamlss package
# in which we modify some graphical parameters. To
# load the modifed function run the next code:
source("my_wp.R")

pdf('Figs/bruha_worm_plots.pdf', width=7, height=7)
par(mfrow=c(2, 2))
my_wp(mod_GA_2,    main='Wormplot for GA model', col="black")
my_wp(mod_IG_2,    main='Wormplot for IG model', col="black")
my_wp(mod_LOGNO_2, main='Wormplot for LOGNO model', col="black")
my_wp(mod_FWE_3,   main='Wormplot for FWE model', col="black")
dev.off()

# Testing if residuals follow normal distribution
ks.test(res_ga, "pnorm", mean=0, sd=1)
ks.test(res_ig, "pnorm", mean=0, sd=1)
ks.test(res_ln, "pnorm", mean=0, sd=1)
ks.test(res_fwe, "pnorm", mean=0, sd=1)


