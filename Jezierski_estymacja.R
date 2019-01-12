# Wczytanie pakietów 

library(rgdal)
library(sp)
library(gstat)
library(ggplot2)

# 1. Wczytanie danych.

pomiary <- readOGR("dane/pomiary.gpkg")
siatka <- readGDAL("dane/pusta_siatka.tif")
granica_sp <- readOGR(dsn = "dane/wojewodztwa/województwa.shp")

# 2.Eksploracyjna analiza danych.

summary(pomiary)
pomiary@data

# Nowa kolumna 'ID'.

pomiary@data$ID <- seq.int(nrow(pomiary@data))

  # Układ - EPSG: 2180, jedna zmienna - tmin, liczba punktów - 196.

ggplot(pomiary@data, aes(tmin)) + geom_histogram()

# Z histogramu można odczytać występowanie dwóch wartości odstających:

max(pomiary@data$tmin) # -0.8495289 
which.max(pomiary@data$tmin) # w wierszu 42

min(pomiary@data$tmin) # -11.21773
which.min(pomiary@data$tmin) # w wierszu 90
  
  # Usunięcie wartości odstających.

pomiary@data[90, 'tmin'] = NA
pomiary@data[42, 'tmin'] = NA
pomiary = pomiary[!is.na(pomiary$tmin), ]
nrow(pomiary)

  # Ponowna weryfikacja histogramu, z której wynika, że powinniśmy usunąć jeszcze jedną 
  # wartość.

ggplot(pomiary@data, aes(tmin)) + geom_histogram()

min(pomiary@data$tmin) # -9.801772
which.min(pomiary@data$tmin) # w wierszu 5.

  
# Usunięcie wartości odstających.

pomiary@data[5, 'tmin'] = NA
pomiary = pomiary[!is.na(pomiary$tmin), ]
nrow(pomiary)

  # Kolejna weryfikacja histrogramu.

ggplot(pomiary@data, aes(tmin)) + geom_histogram()

  # Na poniższych wykresach można zobaczyć, że nasze dane zawierają wartości lokalnie odstające.
  # Są to wartości -3.329741 oraz -3.6 w punktach o indeksie 174 i 181. Duże niepodobieństwo 
  # wystepuje też pomiędzy punktami o ID 68 i 184, ale może to być spowodowane różnicą wysokości.


spplot(pomiary, "tmin", sp.layout = granica_sp)
cloud = variogram(tmin~1, pomiary, cloud = TRUE)
plot(cloud)
#selected = plot(cloud, digitize = TRUE)
#plot(selected, pomiary, "Spatial")

mapview(pomiary["tmin"])

  # Usunięcie wartości lokalnie odstających.

pomiary@data[174, 'tmin'] = NA
pomiary@data[181, 'tmin'] = NA
pomiary = pomiary[!is.na(pomiary$tmin), ]
nrow(pomiary)

 # Ponowna weryfikacja.

cloud = variogram(tmin~1, pomiary, cloud = TRUE)
plot(cloud)
#selected = plot(cloud, digitize = TRUE)
#plot(selected, pomiary, "Spatial")
mapview(pomiary["tmin"])


  # Usunięcie kolejnej wartości lokalnie odstającej.

pomiary@data[149, 'tmin'] = NA

pomiary = pomiary[!is.na(pomiary$tmin), ]
nrow(pomiary)
  
  # Ponowna weryfikacja.

#selected = plot(cloud, digitize = TRUE)
#plot(selected, pomiary, "Spatial")
mapview(pomiary["tmin"])

  # Po wykonaniu eksploracyjnej analizy danych można przystąpić do kolejnego etapu 
  # analizy - tworzenie modeli semiwariogramu. Ze zbioru zostało usuniętych 6 punktów, w których
  # stwierdzono, że wartość odstają lokalnie lub globalnie. 

# 3. Tworzenie modeli semiwariogramów.

  # Posiadając już poprawne dane można sprawdzić czy badane zjawisko wykazuje anizotropię 
  # przestrzenną poprzez stworzenie mapy semiwariogramu.

varioMap <- variogram(tmin~1, 
                       locations = pomiary,
                        cutoff = 300000,
                        width = 40000, 
                       map = TRUE)
plot(varioMap, threshold = 30, col.regions = topo.colors(n = 40))

  # Na powyżej mapie widać, że zjawisko wykazuje anizotropię, więc następnie tworzymy 
  # semiwariogramy kierunkowe.

varioDirection <- variogram(tmin~1, locations = pomiary, 
                           alpha = c(0, 45, 90, 135),
                             cutoff = 300000)
plot(varioDirection)
plot(varioDirection,plot.numbers = TRUE)

  # Model.

varioDirectionModel <- vgm(psill = 0.65, model = "Gau", range = 95000, 
                           nugget = 0.035, anis = c(0, 0.8))



plot(varioDirection, varioDirectionModel, as.table = TRUE)

  # Ocena modelu.

ocena <- krige.cv(tmin~1,
                  locations = pomiary,
                  model = varioDirectionModel,
                  nmax = 20)

RMSE <- sqrt(mean((ocena$residual) ^ 2))

RMSE


# 4. Estymacja.


kg <- krige(tmin~1, locations = pomiary, newdata = siatka, model = varioDirectionModel)

summary(kg)
spplot(kg["var1.pred"])
spplot(kg["var1.var"])

# 5. Zapisanie plików.

write.csv(RMSE, "Jezierski_estymacja.csv")
writeGDAL(kg["var1.pred"], "Jezierski_estymacja.tif")




