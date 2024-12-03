# Paquetes necesarios
packages <- c("ggplot2", "sf", "data.table", "leaflet", "stringr")

# Instalar paquetes no instalados
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Cargar paquetes
invisible(lapply(packages, library, character.only = TRUE))

#Descargar y cargar datos de ardillas
nyc_squirrels <- fread("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-29/nyc_squirrels.csv")

#Descargar y cargar datos del mapa del parque central NY
url_geojson <- "https://raw.githubusercontent.com/ErikaCruzB/UrbanSquirrels/main/data/NYC_map/CentralPark.geojson"
nyc_map <- st_read(url_geojson)
rm(url_geojson,installed_packages,packages)

#Cambiar formato de NA en colores
nyc_squirrels[is.na(primary_fur_color),primary_fur_color := "Undefined"]

squirrelColorMap<-function(colorSel,dataset){
  #Revisando que los colores sean de tipo character
  if (!is.character(colorSel)){
    stop("Color selection must be a character")
  }
  #Homogeneizando formato de texto
  else {
    colorSel<-str_to_title(colorSel)
  }
  #Todas las opciones de color reportadas en el dataset 
  colOp<-c("Black", "Cinnamon","Gray","Undefined")
  #Revisando que el color este dentro de las opciones
  if (!colorSel %in% colOp) {
    stop("Color must be any of the following: Black, Cinnamon,Gray,Undefined")
  }
  #Asignando colores a todas las opciones de color de las ardillas
  pal <- colorFactor(c("black", "chocolate3","azure4","cyan4"), domain = colOp)
  #filtrar los avistamientos de ardillas de un color
  colorloc<-dataset[primary_fur_color == colorSel,list(long,lat,unique_squirrel_id,date)]
  #Cambiando formato de la fecha 
  colorloc[,date_r := gsub('^([0-9]{2})([0-9]{2})([0-9]{4})', '\\1-\\2-\\3',as.character(date))]
  #Generando mapa
  map<-leaflet(data=colorloc) %>%
    addTiles() %>%
    addCircleMarkers(~long, ~lat, radius = 3,
                     color = ~pal(colorSel),
                     popup = ~date_r,
                     label = ~date_r,
                     stroke = FALSE, fillOpacity = 0.5) %>%
    addProviderTiles("Esri.WorldTopoMap")
  return(map)
}

squirrelColorMap("Undefined",nyc_squirrels)
squirrelColorMap("Black",nyc_squirrels)
squirrelColorMap("cinnamon",nyc_squirrels)
squirrelColorMap("gray",nyc_squirrels)

