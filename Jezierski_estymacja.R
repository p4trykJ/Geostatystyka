# Wczytanie pakietów 

library(rgdal)
library(sp)
library(gstat)
library(ggplot2)

# 1. Wczytanie danych

pomiary <- readOGR("dane/pomiary.gpkg")
siatka <- readGDAL("dane/pusta_siatka.tif")
granica_sp <- readOGR(dsn = "dane/wojewodztwa/województwa.shp")

# 2.Eksploracyjna analiza danych

summary(pomiary)

  # Układ - EPSG: 2180, jedna zmienna - tmin, liczba punktów - 196

ggplot(pomiary@data, aes(tmin)) + geom_histogram()

  # Z histogramu można odczytać występowanie dwóch wartości odstających:

max(pomiary@data$tmin) # -0.8495289 
which.max(pomiary@data$tmin) # w wierszu 42

min(pomiary@data$tmin) # -11.21773
which.min(pomiary@data$tmin) # w wierszu 90
  
  # Usunięcie wartości odstających

pomiary@data[90, 'tmin'] = NA
pomiary@data[42, 'tmin'] = NA
pomiary = pomiary[!is.na(pomiary$tmin), ]

  # Ponowna weryfikacja histogramu, z której wynika, że powinniśmy usunąć jeszcze jedną 
  # wartość.

ggplot(pomiary@data, aes(tmin)) + geom_histogram()

min(pomiary@data$tmin) # -9.801772
which.min(pomiary@data$tmin) # w wierszu 5.

  # Usunięcie wartości odstających

pomiary@data[5, 'tmin'] = NA
pomiary = pomiary[!is.na(pomiary$tmin), ]

  # Kolejna weryfikacja histrogramu

ggplot(pomiary@data, aes(tmin)) + geom_histogram()

  # Na poniższych wykresach można zobaczyć, że nasze dane zawierają wartości lokalnie odstające.

spplot(pomiary, "tmin", sp.layout = granica_sp)
cloud = variogram(tmin~1, pomiary, cloud = TRUE)
plot(cloud)
selected = plot(cloud, digitize = TRUE)
plot(selected, pomiary, "Spatial")

mapview(pomiary["tmin"])

  # Są to wartości -3.329741 oraz -3.6 punktach o indeksie 177 i 181.


