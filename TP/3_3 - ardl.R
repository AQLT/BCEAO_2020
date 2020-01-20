library(dynlm) # Permet de directement faire des manipulations sur les données dans le modèle de prévision
load("Data/bceao.RData")
colnames(ipch_benin)
mod <- dynlm(ensemble ~  transport +
               L(transport, 12) + # On prend le lag d'ordre 12
               diff(transport,1), # On différencie
             data = ipch_benin)
mod
summary(mod)

# Équivalent à :
mod <- dynlm(ensemble ~  transport +
               lag(transport, -12) + # On prend le lag d'ordre 12
               diff(transport,1), # On différencie
             data = ipch_benin)
predict(mod) # Prévisions
end(ipch_benin)
