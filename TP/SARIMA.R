library(forecast)
load("Data/bceao.RData")
arima_auto <- auto.arima(ipch_benin[,"ensemble"])
# On peut aussi redéfinir le modèle à main :
arima_fixe <- Arima(ipch_benin[,"ensemble"],
                    order = c(2,1,1), seasonal = c(0,0,2),
                    include.drift = TRUE)
arima_fixe
summary(arima_fixe)

# Avantage de forecast : plus de diagnostics associés :
accuracy(arima_auto)
forecast(arima_auto, h = 10) # donne les prévisions sur 10 périodes

library(RJDemetra)
arima_auto_jd <- regarima_x13(ipch_benin[,"ensemble"])
arima_auto_jd
summary(arima_auto_jd)
#Pour obtenir les prévisions : 
arima_auto_jd$forecast

#Pour avoir une plus grande période, il faut refaire une specification
arima_auto_jd_spec <- regarima_spec_x13(arima_auto_jd,
                                        fcst.horizon = -3) # -3 pour trois années, équivalent ) 12*3
arima_auto_jd <- regarima(ipch_benin[,"ensemble"], arima_auto_jd_spec)
arima_auto_jd$forecast

# Pour spécifier son propre modèle arima
arima_fixe_jd_spec <- regarima_spec_x13(arima_auto_jd,
                                        automdl.enabled = FALSE,
                                        arima.mu = FALSE,
                                        arima.p = 0,
                                        arima.d = 1,
                                        arima.q = 0,
                                        arima.bp = 0,
                                        arima.bd = 1,
                                        arima.bq = 1)
arima_auto_jd <- regarima(ipch_benin[,"ensemble"], arima_fixe_jd_spec)
arima_auto_jd
arima_auto_jd$forecast
