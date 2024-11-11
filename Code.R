

###############################################################################################################

# I) Data Import and Preliminary Analysis

###############################################################################################################


####################### 1: Import Data: ########################################################################

#1. Import Data:

start = as.Date("2022-01-01")
end = as.Date("2024-08-31")
library(quantmod)

######################### Euro Dollar #########################################################################

symbol_eur_doll = "EURUSD=X"

euro_doll_data = getSymbols(symbol_eur_doll, src = "yahoo", from = start, to = end, auto.assign = FALSE)

# On choisit la colone des prix à la fermeture comme série des prix
euro_doll_val  = euro_doll_data[,4]

########################### Gold USD #########################################################################

symbol_gold = "GC=F"

gold_data = getSymbols(symbol_gold, src = "yahoo", from = start, to = end, auto.assign = FALSE)

#On choisit la colone des prix à la fermeture comme série des prix
gold_val = gold_data[,4]

################# DATA CLEANING AND PREPROCESSING #########################################################################

# On choisit de supprimer les valuers manquantes de la série pour les euro_doll

gold_val <- na.omit(gold_val)

# On choisit de supprimer les valuers manquantes de la série pour les euro_doll

euro_doll_val <- na.omit(euro_doll_val)

# On convertit nos données avec une fréquence de 252 pour faire ressortir  uniqument les jours d'ouverture du marcher

ts_euro_doll <- ts(euro_doll_val, start = c(2022, 1), frequency = 252)  
ts_gold <- ts(gold_val, start = c(2022, 1), frequency = 252)

###############################################################################################################

####################### 2: Plot the Data: #######################################################################

# On plot les deux ts: 

par(mfrow = c(2, 1)) 

plot(euro_doll_val, main="Taux de change en Cloture EUR/USD", xlab="Date", ylab="EUR/USD", col="blue")

grid()

plot(gold_val, main="Prix de Cloture de l'Or en USD", xlab="Date", ylab="Prix de l'Or", col="gold")

grid()

###############################################################################################################

#################### 3. Descriptive Analysis: #######################################################################

######################### Euro Dollar #########################################################################

library(e1071)

summary(ts_euro_doll)

skewness(ts_euro_doll)

kurtosis(ts_euro_doll)

#------------- Résulats: -------------#

# Index                EURUSD=X.Close  
# Min.   :2022-01-03   Min.   :0.9596  
# 1st Qu.:2022-08-31   1st Qu.:1.0582  
# Median :2023-05-02   Median :1.0791  
# Mean   :2023-05-02   Mean   :1.0719  
# 3rd Qu.:2024-01-01   3rd Qu.:1.0919  
# Max.   :2024-08-29   Max.   :1.1457 

# > skewness(ts_euro_doll)
#[1] -0.8765443

# > kurtosis(ts_euro_doll)
# [1] 0.816749

#------------------------------------#

#---- Interprétation des valeurs: ------------------------------------------------------------------------#

#     skewness = -0.8765443

#     Cela signifie que que la distribution serait faiblement assymétrique à Gauche:
#     Il serait donc legerement plus fréqent d'avoir des valeurs inferieur à la moyenne

#     kurtosis = 0.816749 

#.    Cela signifie moins de valeur extremes et donc que la plupart sont situés au niveau de la moyenne. 

#.    Ces résultats sont cohérents étant donné la proximité de Q1, de la moyenne,
#.    de la median et de Q3 qui indique un forte concentration autour de la moyenne 
#.    et donc un faible impact des valeurs extremes 

#.    Ces résultats sont cohérents avec la nature du sous-jacent. 
#.    En effet, un taux de change entre deux monnaies mondiales est 
#.    logiquement stable par sa dépéndance à l'économie réel.

#---------------------------------------------------------------------------------------------------------#

######################### Gold USD #########################################################################

summary(ts_gold)

skewness(ts_gold)

kurtosis(ts_gold)

#------------- Résulats: -------------#

# Index                GC=F.Close  
# Min.   :2022-01-03   Min.   :1623  
# 1st Qu.:2022-09-01   1st Qu.:1827  
# Median :2023-05-03   Median :1933  
# Mean   :2023-05-03   Mean   :1970  
# 3rd Qu.:2024-01-01   3rd Qu.:2026  
# Max.   :2024-08-30   Max.   :2526  

# > skewness(ts_gold)
#[1] 0.8796226

# > kurtosis(ts_gold)
# [1] 0.09136757

#------------------------------------#

#---- Interprétation des valeurs: ------------------------------------------------------------------------#

#     skewness = 0.8796226

#     Cela signifie que que la distribution serait faiblement assymétrique à Droite:
#     Il serait donc plus legerement plus fréqent d'avoir des valeurs superieur à la moyenne

#.    On remarque une presque parfaite symétrie avec le eur_usd_clean

#     kurtosis = 0.09136757 

#.    Cela signifie par sa valeur très faible presque aucun impact des valeur extremes et 
#.    donc que la plupart sont situés au niveau de la moyenne. 


#.    Ces résultats sont cohérents étant donné la proximité encor plus prononcé de Q1, de la moyenne, 
#.    de la median et de Q3 qui indique un forte concentration autour de la moyenne et donc un faible 
#.    impact des valeurs extremes. 

#---------------------------------------------------------------------------------------------------------#

########### Analyze any apparent trends, seasonality, or volatility visually.#####################################

######################### Euro Dollar ############################################################################

#--------- Trends : ---------------------------------------------------------------------------------------------#

#     On Observe Jan 2022 à Oct 2023 une tendance baissière assez imortante de 1.13 à environ 0.95
#.    De Oct 2023 à Apr 2023 une très forte tendance haussière qui vient recouvir jusqu'à 1.10 
#.    De Apr 2023 à Jul 2024 une stabilisation autour de 1.075 avec des flucatuations allant de 1.10 à 1.05

#------- Seasonality : ------------------------------------------------------------------------------------------#

#     Il n'y a pas de pathernes équivoques sur le graphique. Nous pouvons cependant voir une répétition 
#.    du motif de Jan 2022 à Jan 2023 au niveau de Jul 2023 à 2024 mais de manière moins prononcé.

#------- Volatility :  : ----------------------------------------------------------------------------------------#

#     la volatilité est très importante sur l'année de 2022 et beaucoup plus raisonné sur l'année 2023

######################### Gold USD  ############################################################################

#--------- Trends : ---------------------------------------------------------------------------------------------#

#     On Observe surtout une tendance global haussière de Jan 2023 à Jul 2024 allant de 1600 à 2500
#.    Celle-ci est vraiment présente de Sep 2023 à Jul 2024
#.    On peut aussi mettre en avant une tendance baissière global sur l'année 2022

#------- Seasonality : ------------------------------------------------------------------------------------------#

#     Il n'y a pas de pathernes équivoques sur le graphique. On peut difficilement mettre en vant une 
#.    quelconque saisonalité. 

#------- Volatility :  : ----------------------------------------------------------------------------------------#

#     la volatilité est très importante sur l'intégralité du graphique aussi bien localement que globalement. 
#     par exemple le prix passe de 2000 à 2400 en moins de deux mois soit 20 % de hausse 

######################### CONCLUSION  ############################################################################

#    Il est diffilce de mettre en évidence une saisonalité particulière, les deux graphes présentent des 
#    tendances baissière puis haussière finalement, pour la volatilité les deux graphes semblent en opposition:
#    Lorsque la volatilité de EUR/USD est forte - la volatilité de l'or est moins importante
#    Lorsque la volatilité de EUR/USD est cosntante voir diminue - la volatilité de l'or est très importante

###################################################################################################################

#################### 4. Stationarity Check: #######################################################################

#    On effectue un test pour connaitre la stationnarité ou non des séries.
#    Etant donné que la variance ne semble pas constante et la moyenne non plus 
#    il y a peu de chance pour qu'elles le soient.

library(tseries)

adf.test(ts_euro_doll)

#------------- Résulats: -------------#

# Augmented Dickey-Fuller Test

# data:  ts_euro_doll
# Dickey-Fuller = -2.6573, Lag order = 8, p-value = 0.3001
# alternative hypothesis: stationary

#------------------------------------#

#---- Interprétation des valeurs: ------------------------------------------------------------------------#

# On a une p-value qui est trop importante pour accepter l'hypothèse que la série est stationnaire

#---------------------------------------------------------------------------------------------------------#

adf.test(ts_gold)

#------------- Résulats: -------------#

# Augmented Dickey-Fuller Test

# data:  ts_gold
# Dickey-Fuller = -1.5494, Lag order = 8, p-value = 0.769
# alternative hypothesis: stationary

#------------------------------------#

#---- Interprétation des valeurs: ------------------------------------------------------------------------#

# On a une p-value qui est trop importante pour accepter l'hypothèse que la série est stationnaire

#---------------------------------------------------------------------------------------------------------#

##################### Differentiate time series ###########################################################

ts_euro_doll_diff_1 = diff(ts_euro_doll)

adf.test(ts_euro_doll_diff_1)

#------------- Résulats: -------------#

# Augmented Dickey-Fuller Test

# data:  ts_euro_doll_diff_1
# Dickey-Fuller = -8.8591, Lag order = 8, p-value = 0.01
# alternative hypothesis: stationary

# Warning message:
#  In adf.test(ts_euro_doll_diff_1) : p-value smaller than printed p-value

#------------------------------------#

#---- Interprétation des valeurs: ------------------------------------------------------------------------#

# On a une p-value qui est très faible nous permettant de rejeter H0 et 
# donc de supposer la série stationnaire

#---------------------------------------------------------------------------------------------------------#

ts_gold_diff_1 = diff(ts_gold)

adf.test(ts_gold_diff_1)

#------------- Résulats: -------------#

# Augmented Dickey-Fuller Test

# data:  ts_euro_doll_diff_1
# Dickey-Fuller = -8.4553, Lag order = 8, p-value = 0.01
# alternative hypothesis: stationary

# Warning message:
#  In adf.test(ts_euro_doll_diff_1) : p-value smaller than printed p-value

#------------------------------------#

#---- Interprétation des valeurs: ------------------------------------------------------------------------#

# On a une p-value qui est très faible nous permettant de rejeter H0 et 
# donc de supposer la série stationnaire

#---------------------------------------------------------------------------------------------------------#

######################### CONCLUSION  #####################################################################

#.  Nos deux séries gold_usd et euro_doll deoivent etre différenciées une fois pour être stationnaires


###########################################################################################################

# II Modeling and Analysis

##########################################################################################################


########### 1. Autoregressive and Moving Average Models: ###################################################

######################### Euro Dollar ######################################################################

acf(ts_euro_doll_diff_1, main="ACF de EUR/USD différencié")

pacf(ts_euro_doll_diff_1, main="PACF de EUR/USD différencié")

#---- Interprétation des valeurs: ------------------------------------------------------------------------#

# Pour l'ACF de eur_usd_diff on a uniquement rho(0) sinon tous les pics sont direcments dans la zone de rejet
# Pour le PACF on peut tirer aucun information

# Les deux graphiques nous démontrent aucun signe d'autocorellation

# Les diagrammes ACF et PACF ne nous permettent pas de faire une hypothèse certaine. 
# Nous allons devoir faire une distinction entre un airima(0,1, 0), arima(0, 1, 1) ou un arima( 1, 1, 0)
# Equivalent d'un AR(1) ou MA(1)

#---------------------------------------------------------------------------------------------------------#

######################### Gold USD ######################################################################

acf(ts_gold_diff_1, main="ACF de GOLD/USD différencié")

pacf(ts_gold_diff_1, main="ACF de GOLD/USD différencié")

#---- Interprétation des valeurs: ------------------------------------------------------------------------#

# Pour l'ACF de gold_usd_diff on a uniquement rho(0) sinon tous les pics sont directements dans la zone de rejet
# Pour le PACF on peut tirer aucun information, aucune rupture brut.

#---------------------------------------------------------------------------------------------------------#

# Les diagrammes ACF et PACF ne nous permettent pas de faire une hypothèse certaine. 
# Nous allons devoir faire une distinction entre un airima(0,1, 0), arima(0, 1, 1) ou un arima( 1, 1, 0)
# Equivalent d'un AR(1) ou MA(1)

############################################################################################################

##### Determine the appropriate AR or MA model order for each series using AIC/BIC criteria ###############

library(forecast)

######################### Euro Dollar ######################################################################

# On fait manuellement trois ARIMA différents pour les comparer
fit_arima_010 <- arima(ts_euro_doll, order = c(0, 1, 0))  # ARIMA(0,1,0)
fit_arima_110 <- arima(ts_euro_doll, order = c(1, 1, 0))  # ARIMA(1,1,0)
fit_arima_011 <- arima(ts_euro_doll, order = c(0, 1, 1))  # ARIMA(0,1,1)

# On récupère les AIC et BIC de nos trois modélisations
aic_values <- c(fit_arima_010$aic, fit_arima_110$aic, fit_arima_011$aic)
bic_values <- c(BIC(fit_arima_010), BIC(fit_arima_110), BIC(fit_arima_011))

aic_values
bic_values

#------------- Résulats: -------------#

#> aic_values
#[1] -5292.755 -5291.203 -5291.258
#> bic_values
#[1] -5288.212 -5282.118 -5282.173

#------------------------------------#

#---- Interprétation des valeurs: ------------------------------------------------------------------------#

# Si on choisit le modèle avec le plus faible BIC et AIC on devrait donc partir sur un ARIMA (0, 1, 0) pour le eur_usd

# Conclusion :     eur_usd devrait etre modélisé par un ARIMA(0, 1, 0)

#---------------------------------------------------------------------------------------------------------#

# On test avec la fonction auto.arima

auto.arima(ts_euro_doll)

#------------- Résulats: -------------#

#> auto.arima(eur_usd_clean)
# Series: eur_usd_clean 
# ARIMA(0,1,0) 

# sigma^2 = 2.846e-05:  log likelihood = 2647.38
# AIC=-5292.75   AICc=-5292.75   BIC=-5288.21

#------------------------------------#

#---- Interprétation des valeurs: ------------------------------------------------------------------------#

# On voit que notre modéle prédit avec le PACF et l'ACF pour l'eur_usd est cohérent avec la fonction auto.arima 

#---------------------------------------------------------------------------------------------------------#

######################### Gold USD ######################################################################

# On fait manuellement trois ARIMA différents pour les comparer
fit_arima_010_gold <- arima(ts_gold, order = c(0, 1, 0))  # ARIMA(0,1,0)
fit_arima_110_gold <- arima(ts_gold, order = c(1, 1, 0))  # ARIMA(1,1,0)
fit_arima_011_gold <- arima(ts_gold, order = c(0, 1, 1))  # ARIMA(0,1,1)

# On récupère les AIC et BIC de nos trois modélisations
aic_values <- c(fit_arima_010_gold$aic, fit_arima_110_gold$aic, fit_arima_011_gold$aic)
bic_values <- c(BIC(fit_arima_010_gold), BIC(fit_arima_110_gold), BIC(fit_arima_011_gold))


aic_values
bic_values

#------------- Résulats: -------------#

# > aic_values
# [1] 5774.333 5773.773 5773.627
# > bic_values
# [1] 5778.838 5782.782 5782.635

#------------------------------------#

#---- Interprétation des valeurs: ------------------------------------------------------------------------#

# On remarque que le plus petit AIC renvoie au troisème modèle Arima(0,1,1) 
# alors que le BIC renvoie au premier modèle Arima(1,1,0)

#---------------------------------------------------------------------------------------------------------#

# On test avec la fonction auto.arima

auto.arima(ts_gold)

#------------- Résulats: -------------#
#Coefficients:
#   ar1      ar2      ma1     ma2   drift
# 0.4737  -0.9303  -0.5262  0.9319  1.0294
# s.e.  0.0423   0.0382   0.0403  0.0410  0.6715

# sigma^2 = 325.8:  log likelihood = -2878.13
# AIC = 5768.25   AICc = 5768.38   BIC = 5795.28

#------------------------------------#

#---- Interprétation des valeurs: ------------------------------------------------------------------------#

# On trouve un AIC plus interessant mais un BIC plus important, on noatteat que les std.e sont plus faibles 

#---------------------------------------------------------------------------------------------------------#

########## Fit AR and ARMA models to the series. ##########################################################

euro_doll_ARIMA_model <- arima(ts_euro_doll, order = c(0, 1, 0))  # ARIMA(0,1,0)

gold_ARIMA_model = arima(ts_gold, order = c(2, 1, 2))  # ARIMA(2,1,2)

############################################################################################################

##################### 2. Residual Analysis:#################################################################

# Perform residual diagnostics (plot residuals, ACF of residuals, Ljung-Box test) to ensure the
# model fits well.

#------ Calcul des résidus ---------------#

euro_doll_ARIMA_model_res = residuals(euro_doll_ARIMA_model)

gold_ARIMA_model_res <- residuals(gold_ARIMA_model)

#-----------------------------------------#

#--- On plot les résidus -----------------#

plot(euro_doll_ARIMA_model_res, main="Résidus du modèle ARIMA(0, 1, 0) EUR/USD", col="blue")

plot(gold_ARIMA_model_res, main="ACF des résidus ARIMA(2, 1, 2) GOLD/USD", col='red')

#-----------------------------------------#

#--- Interprétation des résultats --------#

# On observe que la variance des résidus n'est pas constante 
# Il va falloir utilisé un modèle prenant en compte l'Hétéroscédasticité

#-----------------------------------------#

#--- On fait ACF/PACF pour les rédisus ---#

acf(euro_doll_ARIMA_model_res)
pacf(euro_doll_ARIMA_model_res)

acf(gold_ARIMA_model_res)
pacf(gold_ARIMA_model_res)

#-----------------------------------------#

#--- Interprétation des résultats --------#

# un ACF montrant uniquement une corrélation avec lui meme 
# soit un seul pic en rho(O) qui  suggérerait :
# une indépendance de Epsilon_t avec les précédents.

# Il n'y a donc pas d'autocorrelation dans la série des résidus 

# Meme interprétation pour les PACF qui ne montrent aucun 
# signe de corrélation évidente

# Cela nous permet de conclure que nos deux modèles sont bien
# paramétré et représentent bien nos données initiales

# En effet, l'absence de corrélation montre que l'intégralité des 
# pathernes ont été capturé dans nos modèles ARIMA

#-----------------------------------------#

#--------- ILjung-Box test ---------------#

Box.test(euro_doll_ARIMA_model_res, lag = 10, type = "Ljung-Box")

Box.test(gold_ARIMA_model_res, lag = 10, type = "Ljung-Box")

#-----------------------------------------#

#------------- Résulats: -----------------#

# data:  euro_doll_ARIMA_model_res
# X-squared = 14.469, df = 10, p-value = 0.1527

# data:  gold_ARIMA_model_res
# X-squared = 10.1, df = 10, p-value = 0.4318

#-----------------------------------------#

#--- Interprétation des résultats --------#

# On a que des p-values > 0.05 dans nos deux modèles
# On peut donc conclure que nos résidus ne présentent pas d'autocorrelation.

#-----------------------------------------#

#------------- Conclusion ----------------#

# Nos modèles:

#.    - ARIMA(2,1,2) pour le GOLD / USD
#.    - ARIMA(0,1,0) pour le EUR / USD

# Capturent l'intégralité des informations dans nos données
# Ils sont donc bien adpatés

#-----------------------------------------#

############################################################################################################

################ 3. Heteroscedasticity Testing ############################################################

library(FinTS)

#--- On effectue un test d'Heteroscedasticité ---#

ArchTest(euro_doll_ARIMA_model_res, lags = 10)

ArchTest(gold_ARIMA_model_res, lags = 10)

#------------- Résulats: -----------------#

# ARCH LM-test; Null hypothesis: no ARCH effects

# data:  euro_doll_ARIMA_model_res
# Chi-squared = 25.363, df = 10, p-value = 0.004699

# data:  gold_ARIMA_model_res
# Chi-squared = 34.62, df = 10, p-value = 0.0001449

#-----------------------------------------#

#--- Interprétation des résultats --------#

# On a que des p-values < 0.05 dans nos deux modèles
# On peut donc conclure que nos résidus présentent de l'Heteroscedasticité.

# La variance de nos résidus n'est pas constante, il y a une dispersion

#-----------------------------------------#

## Interpret the results and discuss the presence of volatility clustering ##########################

# Etant donné la présence du phénomène d'Heteroscedasticité il parait plus que probalbe
# de voir apparaitre de "cluster de volatilité" 

# En effet, comme la variance des résidus n'est pas constante, la vairiance devient conditionnelle
# et comme les chocs passés ainsi que leur vélocité impactent la variance future il semble cohérent 
# de supposer la présence de péiode de forte volatilité er d'autres plus calme

# Ce constat est cohérent avec la persepctive macro des ces données.
# On peut souvent apercevoir des périodes de crises et d'acalmie sur les marcher financier

############################################################################################################


###########################################################################################################

# III ARCH and GARCH Model

###########################################################################################################


############################################################################################################

############### 1. Fitting ARCH Models: ###################################################################

library(rugarch)

######################### Euro Dollar ######################################################################

#------ Spécification du modèle ARCH(1) pour EUR/USD -----------#

arch_spec_euro_doll <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 0)),
                                  mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
                                  distribution.model = "norm")

#---------------------------------------------------------------#

#--- Ajustement du modèle ARCH(1) sur les résidus de EUR/USD ---#

arch_fit_euro_doll <- ugarchfit(spec = arch_spec_euro_doll, data = euro_doll_ARIMA_model_res)

#---------------------------------------------------------------#

#------------- Résulats: -----------------#

print(arch_fit_euro_doll)

#*---------------------------------*
#*          GARCH Model Fit        *
#*---------------------------------*
#  
#  Conditional Variance Dynamics 	
#-----------------------------------
#  GARCH Model	: sGARCH(1,0)
#Mean Model	: ARFIMA(0,0,0)
#Distribution	: norm 
#
#Optimal Parameters
#------------------------------------
#             Estimate  Std. Error  t.  value Pr(>|t|)
#omega   0.000026    0.000002  14.3368 0.000000
#alpha1  0.102523    0.049998   2.0505 0.040313
#
# Information Criteria
# ------------------------------------
#   
# Akaike       -7.6331
# Bayes        -7.6200
# Shibata      -7.6331
# Hannan-Quinn -7.6280

#-----------------------------------------#

#--- Interprétation des résultats --------#

# On a un alpha0 / Omega = 0 avec un std.E très faible
# cela est cohérent dans le sens où un taux de change entre 
# deux monnaies mondiales flucute en moyenne très peu
# Avoir une variance en moyenne proche de 0 est cohérent

# Alpha 1 =0.102523 avec une Std.E = 0.049998 ce qui est important

# Les deux p-values sont faibles ce qui nous permet 
# d'assurer une significativité des paramètres dans le 
# modèle

#-----------------------------------------#

######################### Gold USD ######################################################################

#--- Spécification du modèle ARCH(1) pour GOLD/USD -------------#

arch_spec_gold <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 0)),
                             mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
                             distribution.model = "norm")

#---------------------------------------------------------------#

#--- Ajustement du modèle ARCH(1) sur les résidus de GOLD/USD ---#

arch_fit_gold <- ugarchfit(spec = arch_spec_gold, data = gold_ARIMA_model_res)

#---------------------------------------------------------------#

#------------- Résulats: -----------------#

print(arch_fit_gold)

#*---------------------------------*
#  *          GARCH Model Fit        *
#  *---------------------------------*
#  
#  Conditional Variance Dynamics 	
#-----------------------------------
#  GARCH Model	: sGARCH(1,0)
#Mean Model	: ARFIMA(0,0,0)
#Distribution	: norm 
#
#Optimal Parameters
#------------------------------------
#  Estimate  Std. Error   t value Pr(>|t|)
#omega     323.56   23.147967 13.977948        0
#alpha1      0.00    0.046835  0.000004        1

#Information Criteria
#------------------------------------
#  
#Akaike       8.6227
#Bayes        8.6362
#Shibata      8.6227
#Hannan-Quinn 8.6279

#-----------------------------------------#

#--- Interprétation des résultats --------#

# On a un alpha0 / Omega = 323.56 avec un std.E 23.15

# cela est cohérent dans le sens où l'or est un actif 
# qui flucute en moyenne de manière très importante
# Avoir une variance en moyenne élevé e est cohérent

# La très faible p-value (0) montre le poids de ce 
# paramètre au sein du modèle

# Alpha 1 =0.00 avec une Std.E = 0.046835 

# La p-vlaue de 1 montre que ce paramètre ne participe
# presque en rien à la significativité du modèle

#-----------------------------------------#

###### Vérification de la capture de la volatilité par ARCH(1) ######################################

#----- On récupère les résidus que l'on divise par la variance conditionnel-----------#

euro_doll_res_std <- residuals(arch_fit_euro_doll, standardize = TRUE)

gold_res_std <- residuals(arch_fit_gold, standardize = TRUE)

#---- On trace les résidus non standaridés et ceux standardisés pour les comparer ----#

par(mfrow = c(4, 1), mar = c(4, 4, 2, 1))  # marges ajustées

plot(euro_doll_res_std, main = "Euro Doll Standardized Residuals", ylab = "Residuals", xlab = "Time")

plot(euro_doll_ARIMA_model_res, main = "Euro Doll Non-Standardized Residuals", ylab = "Residuals", xlab = "Time")

plot(gold_res_std, main = "Gold Standardized Residuals", ylab = "Residuals", xlab = "Time")

plot(gold_ARIMA_model_res, main = "Gold Non-Standardized Residuals", ylab = "Residuals", xlab = "Time")

#--- Interprétation des résultats --------#

# Sur les deux séries temporelles on constate que la série standardisé issues du modèle ARCH(1) n'est
# pas de variance constante. Le modèle ne prédit pas correctement la variance des résidus et ne semble
# pas très adapté

#-----------------------------------------#


#--- Arch test sur les résidus standardisés: ---#

ArchTest(euro_doll_res_std, lags = 10)

ArchTest(gold_res_std, lags = 10)

#------------- Résulats: -----------------#

#ARCH LM-test; Null hypothesis: no ARCH effects

#data:  euro_doll_res_std
#Chi-squared = 20.788, df = 10, p-value = 0.02262

#ARCH LM-test; Null hypothesis: no ARCH effects

#data:  gold_res_std
#Chi-squared = 33.187, df = 10, p-value = 0.0002534

#-----------------------------------------#

#--- Interprétation des résultats --------#

# Comme le laisser penser les resultats du modele ARCh(1,0) pour la série résiduelle euro_doll
# on observe une diminution de l'hétéroscédascticité dans la série résiduel (car la p-value du
# test ARCH passe de 0.0046 à 0.022). Cependant la série résiduelle reste très hétéroscédastique comme la p-value < 0.05.
# Le modèle ARCH(1,0) capture donc très peu la volatilité de la série résiduelle de euro_doll.
# Pour la série résiduelle gold on observe qusiement aucune amélioration avec ARCh(1,0). la série
# reste très hétéroscédastique et ne capture donc pas dutout la volatilité.

#-----------------------------------------#

############### 2. Fitting GARCH Models: ###################################################################

# Fit a GARCH(1,1) model to the series.

######################### Euro Dollar ######################################################################

#------ Spécification du modèle GARCH(1, 1) pour EUR/USD -----------#

arch_11_spec_euro_doll <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                                     mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
                                     distribution.model = "norm")

#---------------------------------------------------------------#

#--- Ajustement du modèle GARCH(1, 1) sur les résidus de EUR/USD ---#

euro_doll_arch_11_fit <- ugarchfit(spec = arch_11_spec_euro_doll, data = euro_doll_ARIMA_model_res)

#---------------------------------------------------------------#

#------------- Résulats: -----------------#

print(euro_doll_arch_11_fit)

# *---------------------------------*
# *          GARCH Model Fit        *
# *---------------------------------*
# Cette section indique qu'un modèle GARCH(1,1) a été ajusté sur les données.

# Conditional Variance Dynamics
# -----------------------------------
# GARCH Model : sGARCH(1,1)

# Mean Model : ARFIMA(0,0,0)

# Distribution : norm

# Optimal Parameters
# ------------------------------------
#           Estimate   Std. Error   t value   Pr(>|t|)
# omega   0.000000    0.000000    0.13556   0.89217
# alpha1  0.017271    0.002720    6.34978   0.00000
# beta1   0.980262    0.002762  354.91888   0.00000

# Robust Standard Errors:

#           Estimate   Std. Error   t value   Pr(>|t|)
# omega   0.000000    0.000004   0.009273   0.99260
# alpha1  0.017271    0.037250   0.463637   0.64291
# beta1   0.980262    0.035189  27.857013   0.00000
# 

# Information Criteria
# ------------------------------------
# Akaike       -7.6893
# Bayes        -7.6697
# Shibata      -7.6893
# Hannan-Quinn -7.6817
# 

#-----------------------------------------#

#--- Interprétation des résultats --------#

# omega : il est très proche de zéro, la variance moyenne des erreurs est extrêmement faible.
# 
# alpha1 :  0.017271 signifie que les chocs récents ont un faible effet sur la volatilité.
# 
# beta1 : Un coefficient élevé de 0.980262 indique une forte persistance de la volatilité 
# (volatilité qui persiste sur le long terme).

# Les p-values :
# omega a une p-value très élevée (0.89217), ce qui signifie qu'il n'est pas statistiquement significatif.
# alpha1 et beta1 ont des p-values très faibles (0.00000), donc ils sont hautement significatifs dans le modèle.

# Avec les erreurs robustes, omega reste non significatif (p-value de 0.99260), et alpha1 devient non significatif également (p-value de 0.64291). 
# Seul beta1 reste significatif avec une très faible p-value, confirmant une forte persistance de la volatilité.

# Les AIC / BIC sont similaires

#-----------------------------------------#

######################### Gold USD ######################################################################

#--- Spécification du modèle GARCH(1, 1) pour GOLD/USD -------------#

arch_11_spec_gold <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                                mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
                                distribution.model = "norm")

#---------------------------------------------------------------#

#--- Ajustement du modèle GARCH(1,1) sur les résidus de GOLD/USD ---#

gold_arch_11_fit <- ugarchfit(spec = arch_11_spec_euro_doll, data = gold_ARIMA_model_res)
#---------------------------------------------------------------#

#------------- Résulats: -----------------#

print(euro_doll_arch_11_fit)

# *---------------------------------*
# *          GARCH Model Fit        *
# *---------------------------------*

# Conditional Variance Dynamics
# -----------------------------------
# GARCH Model : sGARCH(1,1)

# Mean Model : ARFIMA(0,0,0)

# Distribution : norm

# Optimal Parameters
# ------------------------------------
#           Estimate  Std. Error   t value   Pr(>|t|)
# omega   6.864364    3.990052    1.7204    0.085365
# alpha1  0.038324    0.012818    2.9899    0.002791
# beta1   0.941304    0.019975   47.1241    0.000000
#

# Robust Standard Errors:

#           Estimate   Std. Error   t value   Pr(>|t|)
# omega   6.864364    3.619472    1.8965     0.057893
# alpha1  0.038324    0.012325    3.1094     0.001875
# beta1   0.941304    0.016011   58.7910     0.000000
# 
# Information Criteria
# ------------------------------------
# Akaike       8.5877
# Bayes        8.6079
# Shibata      8.5876
# Hannan-Quinn 8.5955
# 
# Ces critères d'information (Akaike, Bayes, Shibata et Hannan-Quinn) sont utilisés pour évaluer le compromis entre la qualité d'ajustement et la complexité du modèle (nombre de paramètres). Des valeurs plus faibles indiquent un meilleur modèle. Ici, l'Akaike (AIC) de 8.5877 est relativement faible, ce qui montre un bon compromis entre ajustement et simplicité du modèle.

#-----------------------------------------#

#--- Interprétation des résultats --------#

# omega :  est de 6.864, ce qui signifie que la variance moyenne est relativement élevée.
# 
# alpha1 :  une valeur de 0.038324,  les chocs passés ont un effet modéré sur la variance conditionnelle.
# 
# beta1 :  une valeur de 0.941304, cela montre une forte persistance, la volatilité est durable 
# et que les chocs affectent la série pendant longtemps.

# Les p-values :
# omega a une p-value de 0.085365, proche du seuil de significativité 
# alpha1 a une p-value de 0.002791, ce qui signifie qu'il est statistiquement significatif.
# beta1 a une p-value extrêmement faible (0.000000), ce qui montre que 
# la persistance est fortement significative dans le modèle.

# Avec les erreurs robustes, omega devient légèrement plus significatif avec une p-value de 0.057893, proche du seuil de 5%. 
# alpha1 et beta1 restent significatifs avec des p-values faibles, confirmant l'importance de ces paramètres.

# Les AIC/BIC sont aussi legerement meilleurs 

#-----------------------------------------#

#----------- Conclusion ------------------#

# Pour les deux séries le modèles Garch(1,1)
# semblent mieux adapté

#-----------------------------------------#

############### 3: Model Validation:: ###################################################################

#----- On récupère les fitted value de nos modèles -----#

euro_doll_garch_11_fitted_val = fitted(euro_doll_arch_11_fit)

gold_garch_11_fitted_val = fitted(gold_arch_11_fit)


#---- On les convertit en série tempo ------------------#

euro_doll_garch_11_fitted_val_ts = ts(euro_doll_garch_11_fitted_val, start = c(2022, 1), frequency = 252)

gold_garch_11_fitted_val_ts = ts(gold_garch_11_fitted_val, start = c(2022, 1), frequency = 252)

par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))  # marges ajustées
plot(euro_doll_garch_11_fitted_val_ts, main = "Euro/Doll fitted values", xlab = "Date", ylab = "Résidus")
plot(euro_doll_ARIMA_model_res, main = "Euro/Doll observed values", xlab = "Date", ylab = "Résidus")
plot(gold_garch_11_fitted_val_ts, main = "Gold fitted values", xlab = "Date", ylab = "Résidus")
plot(gold_ARIMA_model_res, main = "Gold observed values", xlab = "Date", ylab = "Résidus")

#--- Interprétation des résultats --------#

# De manière incompréhensible les fitted value de notre modèle sont toujours nul
# Pour contourner ce problème nous allons nous meme générer les erreurs avec les 
# variances conditionneles issues du modèle garch(1,1)

#-----------------------------------------#

#---  Extraction des sigma conditionnels ----#

sigma_garch_gold <- sigma(gold_arch_11_fit)

sigma_garch_eur_usd <- sigma(euro_doll_arch_11_fit)

#-- Simulation des erreurs ------------------#

set.seed(123)  # Fixer la graine pour reproductibilité

simulated_errors_gold <- rnorm(length(sigma_garch_gold), mean = 0, sd = sigma_garch_gold)

simulated_errors_eur_usd <- rnorm(length(sigma_garch_eur_usd), mean = 0, sd = sigma_garch_eur_usd)

#---- Conversion ts ---------------------#

simulated_errors_gold_ts <- ts(simulated_errors_gold, start = start(sigma_garch_gold), frequency = frequency(sigma_garch_gold))

simulated_errors_eur_usd_ts <- ts(simulated_errors_eur_usd, start = start(sigma_garch_eur_usd), frequency = frequency(sigma_garch_eur_usd))

#---- On plot les erreurs génré contre celle observé ---# 

par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))  # marges ajustées

plot(simulated_errors_eur_usd_ts, main = "Euro/Doll fitted values", xlab = "Date", ylab = "Résidus")
plot(euro_doll_ARIMA_model_res, main = "Euro/Doll observed values", xlab = "Date", ylab = "Résidus")
plot(simulated_errors_gold_ts, main = "Gold fitted values", xlab = "Date", ylab = "Résidus")
plot(gold_ARIMA_model_res, main = "Gold observed values", xlab = "Date", ylab = "Résidus")

#------ Résultats et analyses ---------------------------#

# On constate que les fitted values suivent la même tendance et montrent des fluctuations similaires
# que les résidus observés, cela indique que le modèle GARCH est efficace pour modéliser la volatilité.

#--------------------------------------------------------#

# On effectue un test d'autocorrelation sur les residus du modèle GARCH(1,1) pour valider le modèle.

# Extraire les résidus standardisés du modèle GARCH

euro_doll_garch_res_std <- residuals(euro_doll_arch_11_fit, standardize = TRUE)
gold_garch_res_std <- residuals(gold_arch_11_fit, standardize = TRUE)

# Appliquer le test de Ljung-Box sur les résidus

Box.test(euro_doll_garch_res_std, lag = 10, type = "Ljung-Box")
Box.test(gold_garch_res_std, lag = 10, type = "Ljung-Box")


#------------- Résulats: -----------------#

#            Box-Ljung test (Euro/Doll)

# data:  euro_doll_garch_res_std
# X-squared = 9.1018, df = 10, p-value = 0.5225

#            Box-Ljung test (Gold)
# 
# data:  gold_garch_res_std
# X-squared = 10.895, df = 10, p-value = 0.3657

#-----------------------------------------#

#--- Interprétation des résultats --------#

# Les p-value des deux tests étant largement superieur à 0.05 (0.52 pour euro_doll et 0.36 pour gold)
# On conclue que les residus standardisés du modèle GARCH(1,1) sont biens indépendants non autocorrélés.
# Cela confirme que le modèle GARCH(1,1) est adapté et performant aux deux séries
# temporelles.

#-----------------------------------------#

############################################################################################################


###########################################################################################################

#  IV) Forecasts

###########################################################################################################


############################################################################################################

# Prédiction sur 20 points pour EUR/USD
forecast_euro_doll <- ugarchforecast(euro_doll_arch_11_fit, n.ahead = 20)

# Prédiction sur 20 points pour Gold
forecast_gold <- ugarchforecast(gold_arch_11_fit, n.ahead = 20)

# Affichage des résultats
plot(forecast_euro_doll, which = 1)  # Prévision de la moyenne pour EUR/USD
plot(forecast_gold, which = 1)  # Prévision de la moyenne pour Gold

#------------- Résulats: -----------------#

# Les résultats sont étrnangements nul pour les valeurs 
# mais pas pour les sigma conditionnels

# nous allons donc faire un forcast à la main à la fois des 
# valeurs ARIMA et ensuite des erreurs grace a GARCH

#-----------------------------------------#

par(mfrow = c(1, 1))

######################### Gold USD ######################################################################

#--- Convertir les données historiques en série temporelle --------#

n_obs_gold <- length(gold_val)
ts_gold_val <- ts(as.numeric(gold_val), start = c(2022, 1), frequency = 252)

#--- Prédiction ARIMA pour les 20 prochaines périodes -------------#

arima_forecast_gold <- predict(gold_ARIMA_model, n.ahead = 20, se.fit = TRUE)

#---- Prévision des variances conditionnelles avec GARCH ----------#

garch_forecast_gold <- ugarchforecast(gold_arch_11_fit, n.ahead = 20)

#----- Extraire les variances conditionnelles prévues (sigma) -----#

garch_sigma_forecast_gold <- sigma(garch_forecast_gold)

#--- Simuler des erreurs futures avec la variance conditionnelle prévue ---#

set.seed(123)  # Pour reproductibilité
simulated_errors_gold <- rnorm(20, mean = 0, sd = garch_sigma_forecast_gold)

#--- Combinaison des prévisions (ARIMA + GARCH) ----------#

manual_forecast_gold <- arima_forecast_gold$pred + simulated_errors_gold

#--- Calcul des intervalles de confiance pour ARIMA --------#

arima_upper_95_gold <- arima_forecast_gold$pred + 1.96 * arima_forecast_gold$se
arima_lower_95_gold <- arima_forecast_gold$pred - 1.96 * arima_forecast_gold$se

#--- Créer les séries temporelles pour les intervalles de confiance  --------#
# Ajuster la date de départ pour correspondre à la fin des données historiques

last_obs_gold <- time(ts_gold_val)[length(ts_gold_val)]  # Récupérer la dernière date des données historiques

ts_arima_upper_95_gold <- ts(arima_upper_95_gold, start = last_obs_gold + 1/252, frequency = 252)
ts_arima_lower_95_gold <- ts(arima_lower_95_gold, start = last_obs_gold + 1/252, frequency = 252)
ts_manual_forecast_gold <- ts(manual_forecast_gold, start = last_obs_gold + 1/252, frequency = 252)
ts_arima_forecast_gold <- ts(arima_forecast_gold$pred, start = last_obs_gold + 1/252, frequency = 252)

#--- Visualisation des prévisions avec les données historiques sur un seul graphique --------#

# Plot des données historiques avec les prévisions
plot(ts_gold_val, type = "l", col = "black", ylab = "Gold Price", 
     main = "Manual Forecast with GARCH (Gold) and 95% Confidence Intervals", 
     xlab = "Time", lwd = 2, xlim = c(2022, 2025))

# Ajoutez les prévisions manuelles (ARIMA + GARCH) en rouge (lignes continues)
lines(ts_manual_forecast_gold, type = "l", col = "red", lwd = 2)

# Ajoutez les prévisions ARIMA seules en bleu (lignes continues)
lines(ts_arima_forecast_gold, type = "l", col = "blue", lwd = 2)

# Ajoutez les intervalles de confiance (en bleu clair)
lines(ts_arima_upper_95_gold, type = "l", col = "lightblue", lwd = 2, lty = 2)
lines(ts_arima_lower_95_gold, type = "l", col = "lightblue", lwd = 2, lty = 2)

# Ajout d'une légende pour distinguer les lignes
legend("bottomright", legend = c("Manual Forecast (ARIMA + GARCH)", "ARIMA Forecast", "Historical Data", "95% CI (ARIMA)"), 
       col = c("red", "blue", "black", "lightblue"), lty = c(1, 1, 1, 2), lwd = 2)

#--- Visualisation des prévisions seulement --------#

# Plot des prévisions ARIMA + GARCH (lignes rouges)
plot(ts_manual_forecast_gold, type = "l", col = "red", ylab = "Gold Price", 
     main = "Forecast (ARIMA + GARCH) and ARIMA with 95% Confidence Intervals (Gold)", xlab = "Time", lwd = 2)

# Ajoutez les prévisions ARIMA seules en bleu
lines(ts_arima_forecast_gold, type = "l", col = "blue", lwd = 2)

# Ajout d'une légende pour distinguer les lignes
legend("topright", legend = c("Manual Forecast (ARIMA + GARCH)", "ARIMA Forecast"), 
       col = c("red", "blue"), lty = c(1, 1, 2), lwd = 2)


######################### EUR USD ######################################################################

#--- Convertir les données historiques EUR/USD en série temporelle --------#

n_obs_eur <- length(euro_doll_val)
ts_euro_doll_val <- ts(as.numeric(euro_doll_val), start = c(2022, 1), frequency = 252)

#--- Prédiction ARIMA pour les 20 prochaines périodes EUR/USD -------------#

arima_forecast_euro_doll <- predict(euro_doll_ARIMA_model, n.ahead = 20, se.fit = TRUE)

#---- Prévision des variances conditionnelles avec GARCH pour EUR/USD -----#

garch_forecast_euro_doll <- ugarchforecast(euro_doll_arch_11_fit, n.ahead = 20)

#----- Extraire les variances conditionnelles prévues (sigma) pour EUR/USD -----#

garch_sigma_forecast_euro_doll <- sigma(garch_forecast_euro_doll)

#--- Simuler des erreurs futures avec la variance conditionnelle prévue pour EUR/USD ---#

set.seed(123)  # Pour reproductibilité
simulated_errors_euro_doll <- rnorm(20, mean = 0, sd = garch_sigma_forecast_euro_doll)

#--- Combinaison des prévisions (ARIMA + GARCH) pour EUR/USD ----------#

manual_forecast_euro_doll <- arima_forecast_euro_doll$pred + simulated_errors_euro_doll

#--- Combiner les prévisions avec les données historiques EUR/USD --------#

# Ici, nous conservons uniquement les prévisions pour éviter les décalages visuels
ts_manual_forecast_euro_doll <- ts(manual_forecast_euro_doll, start = c(2024, 191), frequency = 252)
ts_arima_forecast_euro_doll <- ts(arima_forecast_euro_doll$pred, start = c(2024, 191), frequency = 252)

#--- Calcul des intervalles de confiance pour ARIMA --------#

arima_upper_95 <- arima_forecast_euro_doll$pred + 1.96 * arima_forecast_euro_doll$se
arima_lower_95 <- arima_forecast_euro_doll$pred - 1.96 * arima_forecast_euro_doll$se

#--- Créer les séries temporelles pour les intervalles de confiance  --------#

ts_arima_upper_95 <- ts(arima_upper_95, start = c(2024, 191), frequency = 252)
ts_arima_lower_95 <- ts(arima_lower_95, start = c(2024, 191), frequency = 252)

#--- Visualisation des prévisions avec les intervalles de confiance EUR/USD --------#

# Plot des données historiques avec les prévisions
plot(ts_euro_doll_val, type = "l", col = "black", ylab = "EUR/USD Exchange Rate", 
     main = "Manual Forecast with GARCH (EUR/USD) and 95% Confidence Intervals", 
     xlab = "Time", lwd = 2, xlim = c(2022, 2025))

# Ajoutez les prévisions manuelles (ARIMA + GARCH) en rouge (lignes continues)
lines(ts_manual_forecast_euro_doll, type = "l", col = "red", lwd = 2)

# Ajoutez les prévisions ARIMA seules en bleu (lignes continues)
lines(ts_arima_forecast_euro_doll, type = "l", col = "blue", lwd = 2)

# Ajoutez les intervalles de confiance (en bleu clair)
lines(ts_arima_upper_95, type = "l", col = "lightblue", lwd = 2, lty = 2)
lines(ts_arima_lower_95, type = "l", col = "lightblue", lwd = 2, lty = 2)

# Ajout d'une légende pour distinguer les lignes
legend("bottomright", legend = c("Manual Forecast (ARIMA + GARCH)", "ARIMA Forecast", "Historical Data", "95% CI (ARIMA)"), 
       col = c("red", "blue", "black", "lightblue"), lty = c(1, 1, 1, 2), lwd = 2)

#--- Visualisation des prévisions EUR/USD seulement --------#

# Plot des prévisions ARIMA + GARCH (lignes rouges)
plot(ts_manual_forecast_euro_doll, type = "l", col = "red", ylab = "EUR/USD Exchange Rate", 
     main = "Forecast (ARIMA + GARCH) and ARIMA with 95% Confidence Intervals (EUR/USD)", xlab = "Time", lwd = 2)

# Ajoutez les prévisions ARIMA seules en bleu
lines(ts_arima_forecast_euro_doll, type = "l", col = "blue", lwd = 2)

# Ajout d'une légende pour distinguer les lignes
legend("topright", legend = c("Manual Forecast (ARIMA + GARCH)", "ARIMA Forecast"), 
       col = c("red", "blue"), lty = c(1, 1, 2), lwd = 2)

###############################################################################################################

################## 2. Forecast Evaluation ######################################################################

######  Compare the forecasted values to the actual observations #############################################

# Pour le gold et l'eur_usd nous avons des forcast plutot interessant. Bien evidemment ceux-si vont dépendre la
# seed utiliser pour générer les erreurs 

# Le taux de change présente une relative baisse bient que celui-ci smeble se stabiller autour de 1.10

# Pour l'or, celui-ci tend aussi à se stabilser meme si m'on voit que les bornes de confiance à 95%
# sont vraiment très importantes

######  Mean Squared Error (MSE) and Mean Absolute Percentage Error (MAPE) of the forecasts. #################

######################### Gold USD ######################################################################

#--- Extraire les 20 dernières observations réelles et les aligner avec les prévisions ---#

# On extrait les 20 dernières valeurs de la série historique

observed_gold <- tail(ts_gold_val, 20)

#--- Convertir les prévisions en séries temporelles alignées avec les observations ---#

# Pour GOLD
manual_forecast_gold_ts <- ts(manual_forecast_gold, start = start(observed_gold), frequency = frequency(observed_gold))
arima_forecast_gold_ts <- ts(arima_forecast_gold$pred, start = start(observed_gold), frequency = frequency(observed_gold))

#--- Calcul du MSE et MAPE pour GOLD ---#

# Calcul du MSE pour Gold (ARIMA + GARCH)
mse_manual_gold <- mean((observed_gold - manual_forecast_gold_ts) ^ 2)

# Calcul du MSE pour Gold (ARIMA)
mse_arima_gold <- mean((observed_gold - arima_forecast_gold_ts) ^ 2)

# Calcul du MAPE pour Gold (ARIMA + GARCH)
mape_manual_gold <- mean(abs((observed_gold - manual_forecast_gold_ts) / observed_gold)) * 100

# Calcul du MAPE pour Gold (ARIMA)
mape_arima_gold <- mean(abs((observed_gold - arima_forecast_gold_ts) / observed_gold)) * 100

# Affichage des résultats pour Gold
cat("MSE (ARIMA + GARCH, Gold):", mse_manual_gold, "\n")
cat("MSE (ARIMA, Gold):", mse_arima_gold, "\n")
cat("MAPE (ARIMA + GARCH, Gold):", mape_manual_gold, "%\n")
cat("MAPE (ARIMA, Gold):", mape_arima_gold, "%\n")

#------------- Résulats: -----------------#

# > cat("MSE (ARIMA + GARCH, Gold):", mse_manual_gold, "\n")
# MSE (ARIMA + GARCH, Gold): 2736.887 
# > cat("MSE (ARIMA, Gold):", mse_arima_gold, "\n")
# MSE (ARIMA, Gold): 2356.171 
# > cat("MAPE (ARIMA + GARCH, Gold):", mape_manual_gold, "%\n")
# MAPE (ARIMA + GARCH, Gold): 1.633064 %
# > cat("MAPE (ARIMA, Gold):", mape_arima_gold, "%\n")
# MAPE (ARIMA, Gold): 1.532201 %

#-----------------------------------------#

######################### EUR USD ######################################################################

# On extrait les 20 dernières valeurs de la série historique

observed_eur_usd <- tail(ts_euro_doll_val, 20)

#--- Convertir les prévisions en séries temporelles alignées avec les observations ---#

# Pour EUR/USD
manual_forecast_eur_usd_ts <- ts(manual_forecast_euro_doll, start = start(observed_eur_usd), frequency = frequency(observed_eur_usd))
arima_forecast_eur_usd_ts <- ts(arima_forecast_euro_doll$pred, start = start(observed_eur_usd), frequency = frequency(observed_eur_usd))

#--- Calcul du MSE et MAPE pour EUR/USD ---#

# Calcul du MSE pour EUR/USD (ARIMA + GARCH)
mse_manual_eur_usd <- mean((observed_eur_usd - manual_forecast_eur_usd_ts) ^ 2)

# Calcul du MSE pour EUR/USD (ARIMA)
mse_arima_eur_usd <- mean((observed_eur_usd - arima_forecast_eur_usd_ts) ^ 2)

# Calcul du MAPE pour EUR/USD (ARIMA + GARCH)
mape_manual_eur_usd <- mean(abs((observed_eur_usd - manual_forecast_eur_usd_ts) / observed_eur_usd)) * 100

# Calcul du MAPE pour EUR/USD (ARIMA)
mape_arima_eur_usd <- mean(abs((observed_eur_usd - arima_forecast_eur_usd_ts) / observed_eur_usd)) * 100

# Affichage des résultats pour EUR/USD
cat("MSE (ARIMA + GARCH, EUR/USD):", mse_manual_eur_usd, "\n")
cat("MSE (ARIMA, EUR/USD):", mse_arima_eur_usd, "\n")
cat("MAPE (ARIMA + GARCH, EUR/USD):", mape_manual_eur_usd, "%\n")
cat("MAPE (ARIMA, EUR/USD):", mape_arima_eur_usd, "%\n")

#------------- Résulats: -----------------#

# > cat("MSE (ARIMA + GARCH, EUR/USD):", mse_manual_eur_usd, "\n")
# MSE (ARIMA + GARCH, EUR/USD): 0.0001416963 
# > cat("MSE (ARIMA, EUR/USD):", mse_arima_eur_usd, "\n")
# MSE (ARIMA, EUR/USD): 0.000116888 
# > cat("MAPE (ARIMA + GARCH, EUR/USD):", mape_manual_eur_usd, "%\n")
# MAPE (ARIMA + GARCH, EUR/USD): 0.8882546 %
# > cat("MAPE (ARIMA, EUR/USD):", mape_arima_eur_usd, "%\n")
# MAPE (ARIMA, EUR/USD): 0.8610627 %

#-----------------------------------------#

###############################################################################################################

################## 3. Scenario Analysis: ######################################################################

# pour changer de scénaron nous allons changer arbitrairement la seed et faire une nouvelle similation

set.seed(10000000)  # Pour reproductibilité

######################### Gold USD ######################################################################

#--- Convertir les données historiques en série temporelle --------#

n_obs_gold <- length(gold_val)
ts_gold_val <- ts(as.numeric(gold_val), start = c(2022, 1), frequency = 252)

#--- Prédiction ARIMA pour les 20 prochaines périodes -------------#

arima_forecast_gold <- predict(gold_ARIMA_model, n.ahead = 20, se.fit = TRUE)

#---- Prévision des variances conditionnelles avec GARCH ----------#

garch_forecast_gold <- ugarchforecast(gold_arch_11_fit, n.ahead = 20)

#----- Extraire les variances conditionnelles prévues (sigma) -----#

garch_sigma_forecast_gold <- sigma(garch_forecast_gold)

#--- Simuler des erreurs futures avec la variance conditionnelle prévue ---#

simulated_errors_gold <- rnorm(20, mean = 0, sd = garch_sigma_forecast_gold)

#--- Combinaison des prévisions (ARIMA + GARCH) ----------#

manual_forecast_gold <- arima_forecast_gold$pred + simulated_errors_gold

#--- Calcul des intervalles de confiance pour ARIMA --------#

arima_upper_95_gold <- arima_forecast_gold$pred + 1.96 * arima_forecast_gold$se
arima_lower_95_gold <- arima_forecast_gold$pred - 1.96 * arima_forecast_gold$se

#--- Créer les séries temporelles pour les intervalles de confiance  --------#
# Ajuster la date de départ pour correspondre à la fin des données historiques

last_obs_gold <- time(ts_gold_val)[length(ts_gold_val)]  # Récupérer la dernière date des données historiques

ts_arima_upper_95_gold <- ts(arima_upper_95_gold, start = last_obs_gold + 1/252, frequency = 252)
ts_arima_lower_95_gold <- ts(arima_lower_95_gold, start = last_obs_gold + 1/252, frequency = 252)
ts_manual_forecast_gold <- ts(manual_forecast_gold, start = last_obs_gold + 1/252, frequency = 252)
ts_arima_forecast_gold <- ts(arima_forecast_gold$pred, start = last_obs_gold + 1/252, frequency = 252)

#--- Visualisation des prévisions avec les données historiques sur un seul graphique --------#

# Plot des données historiques avec les prévisions
plot(ts_gold_val, type = "l", col = "black", ylab = "Gold Price", 
     main = "Manual Forecast with GARCH (Gold) and 95% Confidence Intervals", 
     xlab = "Time", lwd = 2, xlim = c(2022, 2025))

# Ajoutez les prévisions manuelles (ARIMA + GARCH) en rouge (lignes continues)
lines(ts_manual_forecast_gold, type = "l", col = "red", lwd = 2)

# Ajoutez les prévisions ARIMA seules en bleu (lignes continues)
lines(ts_arima_forecast_gold, type = "l", col = "blue", lwd = 2)

# Ajoutez les intervalles de confiance (en bleu clair)
lines(ts_arima_upper_95_gold, type = "l", col = "lightblue", lwd = 2, lty = 2)
lines(ts_arima_lower_95_gold, type = "l", col = "lightblue", lwd = 2, lty = 2)

# Ajout d'une légende pour distinguer les lignes
legend("bottomright", legend = c("Manual Forecast (ARIMA + GARCH)", "ARIMA Forecast", "Historical Data", "95% CI (ARIMA)"), 
       col = c("red", "blue", "black", "lightblue"), lty = c(1, 1, 1, 2), lwd = 2)

#--- Visualisation des prévisions seulement --------#

# Plot des prévisions ARIMA + GARCH (lignes rouges)
plot(ts_manual_forecast_gold, type = "l", col = "red", ylab = "Gold Price", 
     main = "Forecast (ARIMA + GARCH) and ARIMA with 95% Confidence Intervals (Gold)", xlab = "Time", lwd = 2)

# Ajoutez les prévisions ARIMA seules en bleu
lines(ts_arima_forecast_gold, type = "l", col = "blue", lwd = 2)

# Ajout d'une légende pour distinguer les lignes
legend("topright", legend = c("Manual Forecast (ARIMA + GARCH)", "ARIMA Forecast"), 
       col = c("red", "blue"), lty = c(1, 1, 2), lwd = 2)


######################### EUR USD ######################################################################

#--- Convertir les données historiques EUR/USD en série temporelle --------#

n_obs_eur <- length(euro_doll_val)
ts_euro_doll_val <- ts(as.numeric(euro_doll_val), start = c(2022, 1), frequency = 252)

#--- Prédiction ARIMA pour les 20 prochaines périodes EUR/USD -------------#

arima_forecast_euro_doll <- predict(euro_doll_ARIMA_model, n.ahead = 20, se.fit = TRUE)

#---- Prévision des variances conditionnelles avec GARCH pour EUR/USD -----#

garch_forecast_euro_doll <- ugarchforecast(euro_doll_arch_11_fit, n.ahead = 20)

#----- Extraire les variances conditionnelles prévues (sigma) pour EUR/USD -----#

garch_sigma_forecast_euro_doll <- sigma(garch_forecast_euro_doll)

#--- Simuler des erreurs futures avec la variance conditionnelle prévue pour EUR/USD ---#

simulated_errors_euro_doll <- rnorm(20, mean = 0, sd = garch_sigma_forecast_euro_doll)

#--- Combinaison des prévisions (ARIMA + GARCH) pour EUR/USD ----------#

manual_forecast_euro_doll <- arima_forecast_euro_doll$pred + simulated_errors_euro_doll

#--- Combiner les prévisions avec les données historiques EUR/USD --------#

# Ici, nous conservons uniquement les prévisions pour éviter les décalages visuels
ts_manual_forecast_euro_doll <- ts(manual_forecast_euro_doll, start = c(2024, 191), frequency = 252)
ts_arima_forecast_euro_doll <- ts(arima_forecast_euro_doll$pred, start = c(2024, 191), frequency = 252)

#--- Calcul des intervalles de confiance pour ARIMA --------#

arima_upper_95 <- arima_forecast_euro_doll$pred + 1.96 * arima_forecast_euro_doll$se
arima_lower_95 <- arima_forecast_euro_doll$pred - 1.96 * arima_forecast_euro_doll$se

#--- Créer les séries temporelles pour les intervalles de confiance  --------#

ts_arima_upper_95 <- ts(arima_upper_95, start = c(2024, 191), frequency = 252)
ts_arima_lower_95 <- ts(arima_lower_95, start = c(2024, 191), frequency = 252)

#--- Visualisation des prévisions avec les intervalles de confiance EUR/USD --------#

# Plot des données historiques avec les prévisions
plot(ts_euro_doll_val, type = "l", col = "black", ylab = "EUR/USD Exchange Rate", 
     main = "Manual Forecast with GARCH (EUR/USD) and 95% Confidence Intervals", 
     xlab = "Time", lwd = 2, xlim = c(2022, 2025))

# Ajoutez les prévisions manuelles (ARIMA + GARCH) en rouge (lignes continues)
lines(ts_manual_forecast_euro_doll, type = "l", col = "red", lwd = 2)

# Ajoutez les prévisions ARIMA seules en bleu (lignes continues)
lines(ts_arima_forecast_euro_doll, type = "l", col = "blue", lwd = 2)

# Ajoutez les intervalles de confiance (en bleu clair)
lines(ts_arima_upper_95, type = "l", col = "lightblue", lwd = 2, lty = 2)
lines(ts_arima_lower_95, type = "l", col = "lightblue", lwd = 2, lty = 2)

# Ajout d'une légende pour distinguer les lignes
legend("bottomright", legend = c("Manual Forecast (ARIMA + GARCH)", "ARIMA Forecast", "Historical Data", "95% CI (ARIMA)"), 
       col = c("red", "blue", "black", "lightblue"), lty = c(1, 1, 1, 2), lwd = 2)

#--- Visualisation des prévisions EUR/USD seulement --------#

# Plot des prévisions ARIMA + GARCH (lignes rouges)
plot(ts_manual_forecast_euro_doll, type = "l", col = "red", ylab = "EUR/USD Exchange Rate", 
     main = "Forecast (ARIMA + GARCH) and ARIMA with 95% Confidence Intervals (EUR/USD)", xlab = "Time", lwd = 2)

# Ajoutez les prévisions ARIMA seules en bleu
lines(ts_arima_forecast_euro_doll, type = "l", col = "blue", lwd = 2)

# Ajout d'une légende pour distinguer les lignes
legend("topright", legend = c("Manual Forecast (ARIMA + GARCH)", "ARIMA Forecast"), 
       col = c("red", "blue"), lty = c(1, 1, 2), lwd = 2)

##################################### Analyse ################################################

# On remarque que le scénarion est plus péssimiste avec cette seed

###############################################################################################

########## 4. Forecasting Evaluation Without ARCH Model #######################################

# Prédiction ARIMA pour EUR/USD
arima_forecast_eur_usd_only <- predict(euro_doll_ARIMA_model, n.ahead = 20)

# Prédiction ARIMA pour Gold
arima_forecast_gold_only <- predict(gold_ARIMA_model, n.ahead = 20)

#--- Convertir les prédictions en séries temporelles alignées avec les observations ---#

# Extraire la dernière observation des séries réelles pour aligner les prévisions
last_obs_gold <- time(ts_gold_val)[length(ts_gold_val)]
last_obs_eur_usd <- time(ts_euro_doll_val)[length(ts_euro_doll_val)]

# Conversion des prévisions ARIMA en séries temporelles (sans ARCH/GARCH)
ts_arima_forecast_eur_usd_only <- ts(arima_forecast_eur_usd_only$pred, start = last_obs_eur_usd + 1/252, frequency = 252)
ts_arima_forecast_gold_only <- ts(arima_forecast_gold_only$pred, start = last_obs_gold + 1/252, frequency = 252)

#--- Visualisation des prévisions avec les observations précédentes -------------#

# Pour EUR/USD
plot(ts_euro_doll_val, type = "l", col = "black", ylab = "EUR/USD Exchange Rate", 
     main = "Forecast with ARIMA (EUR/USD) vs Previous Observations", xlab = "Time", lwd = 2, xlim = c(2022, 2025))

lines(ts_arima_forecast_eur_usd_only, col = "blue", lwd = 2)  # Prévision ARIMA seule
legend("bottomright", legend = c("Previous Observations", "ARIMA Forecast"), col = c("black", "blue"), lty = 1, lwd = 2)

# Pour Gold
plot(ts_gold_val, type = "l", col = "black", ylab = "Gold Price", 
     main = "Forecast with ARIMA (Gold) vs Previous Observations", xlab = "Time", lwd = 2, xlim = c(2022, 2025))

lines(ts_arima_forecast_gold_only, col = "blue", lwd = 2)  # Prévision ARIMA seule
legend("bottomright", legend = c("Previous Observations", "ARIMA Forecast"), col = c("black", "blue"), lty = 1, lwd = 2)

###############################################################################################

###############################################################################################

##################################### Analyse ################################################

# Les résulats sont assez moyens 
