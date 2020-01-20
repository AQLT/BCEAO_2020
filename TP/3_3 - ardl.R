library(dynlm) # Permet de directement faire des manipulations sur les données dans le modèle de prévision
load("Data/bceao.RData")
colnames(ipch_benin)
mod <- dynlm(ensemble ~  transport +
               L(transport, 12) + # On prend le lag d'ordre 12
               diff(transport,1), # On différencie
             data = ipch_benin)

# Équivalent à :
mod <- dynlm(ensemble ~  transport +
               lag(transport, -12) + # On prend le lag d'ordre 12
               diff(transport,1), # On différencie
             data = ipch_benin)
mod
summary(mod)
predict(mod) # Prévisions


library(dynamac)
model <- dynardl(ensemble ~ transport + alimentaires,
        lags = list("ensemble"= 1,"transport" = 1),
        diffs = c("alimentaires"), 
        ec = TRUE, simulate = FALSE,
        data = data.frame(ipch_benin))
model
summary(model)
