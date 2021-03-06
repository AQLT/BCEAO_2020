library(ecm)
brent = ts(c(32.5, 30.6, 28.9, 26, 25.4, 24, 26, 38.1, 49.4, 50.1, 
             44.8, 38.6, 32.9, 26.2, 28.1, 29.7, 29.9, 29.5, 31.6, 31.4, 31.7, 
             34.3, 31.5, 26.4, 26.3, 26.8, 26.7, 28.5, 29.3, 30.3, 27.6, 26.3, 
             27.3, 28.5, 29, 27.4, 26.8, 29.2, 29.7, 28.5, 28.4, 27.7, 27.5, 
             27.6, 25.3, 26.5, 25, 22.5, 23.9, 23, 22.6, 24.8, 25.9, 26.4, 
             27, 25.5, 23.9, 24.4, 25.9, 24.5, 24.9, 25.4, 24, 26, 25.9, 24.3, 
             22, 23, 24.2, 22.7, 23.7, 25.6, 25.9, 25.8, 29, 30.8, 28.7, 27.4, 
             28.8, 29.7, 33.1, 35.8, 33, 35.5, 36, 34.1, 31.9, 28.4, 31.1, 
             29, 31.1, 32.4, 31.2, 33.3, 31.5, 28.9, 26.1, 24.3, 22.6, 23.1, 
             24.3, 20.6, 20.8, 20.3, 21.6, 19.9, 17.8, 15.9, 17.9, 17.2, 21.3, 
             26.7, 26.6, 28.3, 34.2, 35.6, 40.1, 38.4, 44.8, 47.1, 46.7, 52.7, 
             53.4, 45, 57.2, 58.5, 56.8, 61.6, 70.8, 67.5, 70.8, 53.8, 50.9, 
             55.8, 50.5, 53.5, 60.8, 60.8, 53.3, 53.5, 52.5, 42.4, 39.6, 39.1, 
             41.2, 43.2, 50.3, 54.1, 51.7, 47, 48.4, 50.8, 54, 52.3, 45.2, 
             51.9, 55.1, 56.8, 52.6, 42.8, 41.7, 44.1, 46.6, 49.8, 45.1, 47.4, 
             46, 45.3, 46.4, 45.4, 51.5, 51.7, 58.3, 54.2, 58.1, 66.1, 66.4, 
             74.4, 62, 55.2, 63.9, 65.1, 75.4, 75.1, 71.5, 84.2, 89.5, 97.6, 
             96.2, 91.5, 87.6, 89.9, 97.5, 94.2, 96.6, 107, 102.3, 101.4, 
             108.7, 107.1, 90.8, 85.6, 86.2, 88.9, 77.3, 82.7, 87.9, 93.2, 
             93.6, 98.8, 104.3, 97.4, 103.6, 108.5, 119, 118.2, 118.9, 121.9, 
             124.7, 131, 149.2, 160.3, 159.1, 143.5, 130.3, 101.4, 79.1, 57.3, 
             62.4, 63.9, 67, 71, 78.9, 92.1, 86, 95.4, 87, 91.9, 96.3, 95.3, 
             100, 100.6, 109, 118.8, 113.6, 114.8, 111, 113, 112.3, 112.5, 
             117, 130.3, 136.5, 141.1, 152.4, 159.4, 149.9, 148.1, 152.9, 
             143.6, 150.6, 148, 152.7, 153.4, 161.5, 167.3, 176.4, 171.2, 
             162.3, 144, 155.5, 169.5, 164.7, 161.1, 159.4, 155.8, 157.7, 
             162.9, 158.2, 148.7, 148.4, 145.8, 153.9, 154.8, 156.3, 150, 
             149.3, 151, 147.8, 148.8, 145.9, 146.1, 148.8, 153.9, 150, 145.3, 
             143.5, 130.9, 121.1, 97.6, 80.6, 95.9, 98.8, 103.5, 107.5, 102.5, 
             96.2, 78.3, 79.4, 80.6, 77.1, 65.4, 52.9, 54.3, 64.4, 68.6, 77.3, 
             80.3, 76.6, 78.1, 79.5, 87.1, 81.6, 97.5, 97.6, 98.4, 92, 93.8, 
             86.9, 79.2, 79.7, 82.2, 87.1, 91.5, 99.9, 101.7, 105.9, 98.9, 
             100.1, 109.8, 121.8, 119.1, 118.8, 117.4, 126.5, 131.9, 106.5, 
             93.2, 97.3, 105.4, 109.4, 118.6, 119, 106.4, 106.5, 99.2, 106.8, 
             101, 107),
           start= 1990, frequency = 12)
raffinage = ts(c(49, 49.4, 48.9, 49.5, 49.9, 49.1, 48.4, 48.7, 49.2, 
                 48.5, 49.1, 49.9, 50.8, 52, 52.7, 53.8, 53, 51.7, 52.6, 52.8, 
                 55, 55.9, 55.4, 55.7, 56.5, 55.8, 54.9, 54.3, 54.6, 54.1, 54.2, 
                 55.8, 55.2, 55.8, 56, 54.7, 53.8, 53.2, 52.2, 52.1, 51.7, 50.6, 
                 50.5, 49.7, 50, 49.9, 49.4, 48.2, 48.5, 48.6, 50.5, 52.7, 52.9, 
                 53.2, 55.9, 57.4, 58.9, 59.8, 62.3, 64.9, 65.6, 67.3, 69.6, 68, 
                 70.7, 72.1, 72.5, 73.6, 79, 78.2, 77.2, 71.4, 67, 67.2, 66.8, 
                 68.7, 70.2, 69.9, 66.9, 65.9, 67, 64.1, 60.6, 59.4, 60.7, 61.1, 
                 64, 66.6, 65.6, 64.1, 64.6, 66.7, 68.5, 68.9, 65.9, 68.1, 70.2, 
                 73.4, 74.5, 66.9, 63.1, 63.6, 64.9, 66.7, 65.2, 65.9, 66.3, 65.6, 
                 67.1, 66.2, 69.1, 70.5, 74, 72.9, 74.2, 77.1, 78, 82.4, 79.9, 
                 76.8, 76.3, 77.9, 82.7, 84.9, 82.8, 87.3, 90.3, 93, 97, 96.6, 
                 90.9, 90.2, 93.2, 93.1, 94.4, 98, 98.2, 98, 99.8, 99.6, 92.9, 
                 90.7, 88.7, 89.1, 87, 88.5, 91.1, 94.3, 96, 96.8, 97.7, 96.6, 
                 98.6, 100.1, 106.4, 107.2, 106, 107.4, 110.6, 114.9, 126.4, 131.3, 
                 131.5, 125.7, 117.9, 105.4, 89.9, 78.3, 76.2, 74.8, 72.9, 75.2, 
                 76.9, 83.6, 83.1, 86.6, 84.3, 86.2, 88.4, 89, 93, 92.9, 96.9, 
                 101.3, 102.1, 102, 98.8, 99, 99.5, 101.3, 103.7, 109.7, 112.6, 
                 116, 122.4, 125.1, 120.6, 118.3, 120.8, 118.6, 121.1, 120.8, 
                 123.8, 123.8, 129.3, 133.1, 136.3, 134.5, 127.4, 119.1, 125.1, 
                 133.4, 134, 132.9, 130, 126.1, 127.5, 131.7, 127.7, 121.5, 120.1, 
                 120.3, 123.4, 124.9, 125.4, 121.7, 122, 123.2, 121.3, 120.8, 
                 119.1, 118.5, 118.8, 120.4, 120, 118.1, 118.2, 111.8, 107, 95.1, 
                 89, 97.2, 99.4, 101.6, 104.4, 103.1, 98.7, 90.5, 89.8, 88.7, 
                 88.6, 81.1, 78.6, 78.2, 83.1, 84.6, 89.5, 91.8, 88.5, 88, 89.2, 
                 94, 91.6, 98.4, 102, 102.3, 99.1, 100.4, 97, 93.6, 94.5, 96, 
                 99.4),
               start = 1995, frequency = 12)
donnees_ts <- ts.intersect(raffinage, brent)
data <- data.frame(donnees_ts)
model <- ecm(data["raffinage"], data["brent"], data["brent"], includeIntercept=TRUE)
model
summary(model)

# La prochaine partie est faite pour extraire les prévisions
data_prev <- ts.intersect(diff(brent, 1),
                      lag(brent, -1))
data_prev <- ts.union(data_prev,
                      lag(raffinage, -1))
data_prev <- window(data_prev, start = start(donnees_ts))
colnames(data_prev) <- c("deltabrent", "brentLag1", "yLag1")
# Il va falloir prévoir la valeur de yLag1 de manière récursive
# Pour cela on récupère d'abord le vecteur des temps des dates qu'on va mettre à jour
# Pour chaque date, les valeurs vont être utilisées pour prévoir le prochain mois
dates <- time(ts(0, start = end(raffinage), end = end(data_prev), frequency = frequency(data_prev)))
dates <- dates[-length(dates)] # On enlève la dernière valeur car on n'a pas de donnée pour le dernier mois
for(date_fin in dates){
  donnes_prev <- data.frame(window(data_prev, end = date_fin)) # s'arrete à date_fin
  der_y_prev <- tail(predict(model, donnes_prev),1) # On récupère la dernière prévision
  window(data_prev,
         start = date_fin+ 1/frequency(data_prev),
         end = date_fin+ 1/frequency(data_prev))[,"yLag1"] <- der_y_prev # On remplace la dernière prévision dans data_prev
}
donnes_prev <- data.frame(data_prev)

donnees_y_prevues <- ts(predict(model, donnes_prev),
                        start = start(data_prev), frequency = 12)
donnees_y_prevues # Prévisions
