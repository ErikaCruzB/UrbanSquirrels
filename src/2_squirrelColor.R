library(readr)
library(sf)

#Descargar y cargar datos de ardillas
nyc_squirrels <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-29/nyc_squirrels.csv")

#Descargar datos del mapa
nyc_map<-read_sf(url("https://github.com/ErikaCruzB/UrbanSquirrels/blob/1d6d645cfc7fd7310fd7d49ecab92de51cc13283/data/NYC_map/ProspectPark.shp"))

