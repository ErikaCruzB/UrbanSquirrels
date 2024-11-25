library(sf)
library(data.table)
library(ggplot2)
library(leaflet)
library(time)

#Descargar y cargar datos de ardillas
nyc_squirrels <- fread("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-29/nyc_squirrels.csv")

#Descargar y cargar datos del mapa del parque central NY
url_geojson <- "https://raw.githubusercontent.com/ErikaCruzB/UrbanSquirrels/main/data/NYC_map/CentralPark.geojson"
nyc_map <- st_read(url_geojson)
rm(url_geojson)

#filtrar los avistamientos de ardillas de un color
colorsq<-"Black"
colorloc<-nyc_squirrels[primary_fur_color == colorsq,list(long,lat,unique_squirrel_id,date)]

#Cambiando formato de la fecha 
colorloc[,date_r := gsub('^([0-9]{2})([0-9]{2})([0-9]{4})', '\\1-\\2-\\3',as.character(date))]

#Mapa de las ardillas de un color
pal <- colorFactor(c("black", "brown"), domain = c("black", "Cinnamon"))

leaflet(data=colorloc) %>%
  addTiles() %>%
  addCircleMarkers(~long, ~lat, radius = 3,
                   color = ~pal(colorsq),
                   popup = ~date_r,
                   label = ~date_r,
                   stroke = FALSE, fillOpacity = 0.5) %>%
  addProviderTiles("Esri.WorldTopoMap")
