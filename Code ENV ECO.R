#Librairie utilisées :
library(tidyverse)
library(readxl) # lire les fichiers excel
library(dplyr)  #L'opérateur "pipe" %>%, fonctions "mutate", "filter"...
library(tidyquant) # données financières
library(ggfortify) # package "autoplot"
library(ggplot2) # graphiques jolies
library(tseries) # test de Phillips-Perron, test KPSS, Test de Jarque-Bera
library(urca) # test de ADF
library(forecast) # ggAcf, ggPacf
library(gridExtra) # poser ensemble les graphiques (dans ce code ggAcf, ggPacf)
library(broom) # Tableaux des résultats pour ARIMA
library(lmtest) # test de DW
library(FinTS) # test ARCH
library(rugarch) # Garch model
library(vars)
library(nlme)

#Chemin pour les bases de données :
setwd("C:/Users/alixv/OneDrive/Bureau/Master/Environnmental Economic")

###############################################
###############################################

#Base de données initiales
df = read.csv2("Prix_energies.csv")
#df <- df[1:204, ]


###############################################
###############################################
                 #ELECTRICITE#
###############################################
###############################################

ts_elec <- ts(df$Prix.Elec.Toute.Tranche, start = c(2007, 1), frequency =12)

# Plot
plot(ts_elec, type = "l", main = "",  xlab = "Année",
     ylab = "Prix électricité (MW/h / €)")

###############################################
###############################################
                    #BOIS#
###############################################
###############################################

ts_bois <- ts(df$Prix.Bois.Tonne.Granule, start = c(2007, 1), frequency = 12)

# Plot
plot(ts_bois, type = "l", main = "Evolution du prix de le tonne de granulé de bois de 2007 à 2024",  xlab = "Année",
     ylab = "Prix granulé (Tonne / €)")

###############################################
###############################################
                  #PETROLE#
###############################################
###############################################

ts_petrole <- ts(df$Prix.Tonne.Propane, start = c(2007, 1), frequency = 12)

# Plot
plot(ts_petrole, type = "l", main = "Evolution du prix de le tonne de propane de 2007 à 2024",   xlab = "Année",
     ylab = "Prix du propane (Tonne / €)")

###############################################
###############################################
                    #GAZ#
###############################################
###############################################

ts_gaz <- ts(df$Prix.Gaz.TTC, start = c(2007, 1), frequency = 12)

# Plot
plot(ts_gaz, type = "l", main = "Evolution du prix du gaz de 2007 à 2024", xlab = "Année",
     ylab = "Prix du gaz (€)")

###############################################
###############################################
                  #INFLATION#
###############################################
###############################################

ts_infla <- ts(df$Inflation, start = c(2007, 1), frequency = 12)

# Plot
plot(ts_infla, type = "l", main = "Evolution de l'inflation de 2007 à 2024", xlab = "Année",
     ylab = "Inflation (Variation en %)")

###############################################
###############################################
                #Part Pard ER#
###############################################
###############################################

ts_er = ts(df$Part.ER.Prod, start = c(2007,1), frequency = 12)

# Plot
plot(ts_er, type = "l", main = "Evolution de la part de production d'électricité par un moyen de production renouvelable (hydraulique, éolien, solaire) de 2007 à 2024", xlab = "Année",
     ylab = "Part de la production totale (%)")

## Saisonnalité ##

# Décomposition des séries
decomp_er <- stl(ts_er, s.window = "periodic")

# Désaisonnalisation
ts_er_deseason <- seasadj(decomp_er)

# Représentation
par(mfrow = c(1,1)
plot(ts_er, main = "Série originale: Part ER dans prod totale")
plot(ts_er_deseason, main = "Série désaisonnalisée: Part ER dans prod totale")

###############################################
###############################################
          #Statistiques Descriptives#
###############################################
###############################################

# Fonction auto pour statistiques descritives
desc <- function(x) {
  c(
    Nb = length(x),
    Min = min(x),
    Median = median(x),
    Mean = mean(x),
    Max = max(x),
    SD  = sd(x)
  )}


#Application
desc(df$Prix.Elec.Toute.Tranche)
desc(df$Prix.Bois.Tonne.Granule)
desc(df$Prix.Tonne.Propane)
desc(df$Prix.Gaz.TTC)
desc(df$Inflation)
desc(df$Part.ER.Prod)

df$Prix.Elec.Toute.Tranche_lag1 <- dplyr::lag(df$Prix.Elec.Toute.Tranche, 1)

ts_elect1 = ts(df$Prix.Elec.Toute.Tranche_lag1)

###############################################
                #REGRESSION#
###############################################

reg = lm(Prix.Elec.Toute.Tranche ~ Part.ER.Prod + Prix.Elec.Toute.Tranche_lag1 + Prix.Bois.Tonne.Granule + Prix.Tonne.Propane + Prix.Gaz.TTC + Inflation, data = df )
summary(reg)

###############################################
                #STATIONNARITE#
###############################################

adf.test(df$Prix.Elec.Toute.Tranche)
adf.test(df$Part.ER.Prod)
adf.test(df$Prix.Bois.Tonne.Granule)
adf.test(df$Prix.Tonne.Propane)
adf.test(df$Prix.Gaz.TTC)
adf.test(df$Inflation)

###############################################
                 #CRITERE BIC#
###############################################

ADFFUEL = ur.df(df$Prix.Tonne.Propane,type="trend",selectlags = "BIC")
summary(ADFFUEL)
ADFFUEL = ur.df(df$Prix.Tonne.Propane,type="drift",selectlags = "BIC")
summary(ADFFUEL)
ADFFUEL = ur.df(df$Prix.Tonne.Propane,type="none",selectlags = "BIC")
summary(ADFFUEL)

ADFELEC = ur.df(df$Prix.Elec.Toute.Tranche,type="trend",selectlags = "BIC")
summary(ADFELEC)
ADFELEC = ur.df(df$Prix.Elec.Toute.Tranche,type="drift",selectlags = "BIC")
summary(ADFELEC)
ADFELEC = ur.df(df$Prix.Elec.Toute.Tranche,type="none",selectlags = "BIC")
summary(ADFELEC)

ADFBOIS = ur.df(df$Prix.Bois.Tonne.Granule,type="trend",selectlags = "BIC")
summary(ADFBOIS)
ADFBOIS = ur.df(df$Prix.Bois.Tonne.Granule,type="drift",selectlags = "BIC")
summary(ADFBOIS)
ADFBOIS = ur.df(df$Prix.Bois.Tonne.Granule,type="none",selectlags = "BIC")
summary(ADFBOIS)

ADFINFLA = ur.df(df$Inflation,type="trend",selectlags = "BIC")
summary(ADFINFLA)
ADFINFLA = ur.df(df$Inflation,type="drift",selectlags = "BIC")
summary(ADFINFLA)
ADFINFLA = ur.df(df$Inflation,type="none",selectlags = "BIC")
summary(ADFINFLA)

ADFER = ur.df(ts_er_deseason,type="trend",selectlags = "BIC")
summary(ADFER)

###############################################
                   #KPSS TEST#
###############################################

kpss.test(df$Prix.Elec.Toute.Tranche)
kpss.test(df$Part.ER.Prod)
kpss.test(df$Prix.Bois.Tonne.Granule)
kpss.test(df$Prix.Tonne.Propane)
kpss.test(df$Prix.Gaz.TTC)
kpss.test(df$Inflation)
kpss.test(df$Part.ER.Prod)

###############################################
              #DIFFERENTIATION#
###############################################

ts_elec_dif=diff(ts_elec)
ADFELEC = ur.df(ts_elec_dif,type = "trend",selectlags = "BIC")
summary(ADFELEC)

ts_elect1_dif=diff(ts_elect1)
ADFELEC = ur.df(ts_elect1_dif,type = "trend",selectlags = "BIC")
summary(ADFELEC)

ts_petrole_dif=diff(ts_petrole)
ADFFUEL = ur.df(ts_petrole_dif,type = "trend",selectlags = "BIC")
summary(ADFFUEL)

ts_bois_dif=diff(ts_bois)
ADFBOIS = ur.df(ts_bois_dif,type = "trend",selectlags = "BIC")
summary(ADFBOIS)

ts_gaz_dif=diff(ts_gaz)
ADFGAZ = ur.df(ts_gaz_dif,type = "trend",selectlags = "BIC")
summary(ADFGAZ)

ts_infla_dif=diff(ts_infla)
ADFINFLA = ur.df(ts_infla_dif,type = "trend",selectlags = "BIC")
summary(ADFINFLA)

ts_er_dif=diff(ts_er)

ADFER = ur.df(ts_er_dif, type = "trend", selectlags = "BIC")
summary(ADFER)

###############################################
            #Test d'autocorélation#
###############################################

Box.test(ts_elec_dif, lag = 20, type = "Ljung-Box")

Box.test(ts_elect1_dif, lag = 20, type = "Ljung-Box")

Box.test(ts_petrole_dif, lag = 20, type = "Ljung-Box")

Box.test(ts_bois_dif, lag = 20, type = "Ljung-Box")

Box.test(ts_gaz_dif, lag = 20, type = "Ljung-Box")

Box.test(ts_infla_dif, lag = 20, type = "Ljung-Box")

Box.test(ts_er_dif, lag = 20, type = "Ljung-Box")

###############################################
###############################################
###############################################
ts_er_deseason=ts_er_deseason[2:222]
ts_er_deseason = ts(ts_er_deseason)

ts_elec=ts_elec[2:222]
tr <- seq(2:204)
df$dum[2:204] <- 0
df$dum[199:204] <- 1

dum=df$dum[2:204]

length(ts_elec)
length(ts_er_dif)
length(ts_elect1_dif)
length(ts_bois_dif)
length(ts_gaz_dif)
length(ts_infla_dif)



reg = lm(ts_elec ~ ts_er_dif + ts_elect1_dif + ts_bois_dif  + ts_gaz_dif + ts_infla_dif)# + ts_elect1_dif + ts_bois_dif + ts_petrole_dif + ts_gaz_dif + ts_infla_dif, data = df)
summary(reg)


###############################################
  #### Test d'autocorrélation des résidus ####
###############################################
residus = resid(reg)
residus = ts(resid(reg), start = start(ts_elec), frequency = frequency(ts_elec)) 
plot(residus)
hist(residus)
acf(residus)
pacf(residus)
Box.test(residus)

## Hétéro
bptest(reg)

## Normalité
shapiro.test(residus)


## Correction 
coeftest(reg, vcov. = NeweyWest(reg, lag = 12, prewhite = FALSE))



        