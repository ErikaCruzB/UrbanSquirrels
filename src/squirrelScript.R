#Autoras: Erika Viridiana Cruz Bonilla y Amairani Cancino Bello
#Descripción: 
#Fecha: 26/11/2024

# Paquetes necesarios
packages <- c("ggplot2", "sf", "data.table", "leaflet",
              "stringr","tidyr","dplyr","viridis")

# Instalar paquetes no instalados
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Cargar paquetes
invisible(lapply(packages, library, character.only = TRUE))

#Descarga de datos:   
nyc_squirrels <- fread("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-29/nyc_squirrels.csv")

## --Pregunta 1----
#' Relación entre la Edad y los Comportamientos de las Ardillas
#'
#' Esta función genera un gráfico de barras para visualizar la relación entre la edad de las ardillas 
#' y su comportamiento con los humanos (ej. correr, trepar, etc.). La función permite elegir el tipo 
#' de comportamiento a analizar y organiza los datos en un formato adecuado para ser graficado.
#'
#' @param data Un data frame que contiene los datos de las ardillas. Este data frame debe tener una columna 
#'             llamada 'age' que indique la edad de la ardilla ('Juvenile' o 'Adult') y columnas adicionales 
#'             que representen los comportamientos de las ardillas, como 'running', 'chasing', etc.
#' @param behavior Un vector de caracteres con los nombres de las actividades a analizar (por defecto es 
#'                 "running", "chasing", "climbing", "eating"). Este parámetro permite seleccionar los 
#'                 comportamientos específicos a graficar.
#'
#' @return Un gráfico de barras que muestra la relación entre la edad de las ardillas y el comportamiento 
#'         de las mismas, donde cada barra representa el número de ardillas de cada edad que participaron 
#'         en el comportamiento especificado.
#'
#' @import ggplot2
#' @import dplyr
#' @import tidyr
#' @export
plot_age_vs_behavior <- function(data, behavior = c("running", "chasing", "climbing", "eating","foraging")) {
  # Convertir la columna 'age' en un factor para asegurar que ggplot lo trate como categorías
  data$age <- as.character(data$age)
  data$age[!data$age %in% c('Juvenile', 'Adult')] <- NA
  data$age <- factor(data$age, levels = c('Juvenile', 'Adult'))
  
  # Filtrar los datos para eliminar NA en 'age' y 'behavior'
  #Contar las observaciones por comportamiento y transformar a porcentaje
  data_filtered <- data %>%
    select(age, all_of(behavior)) %>%
    pivot_longer(cols = all_of(behavior), names_to = "behavior", values_to = "engaged") %>%
    filter(engaged == TRUE) %>%
    filter(!is.na(age)) %>% 
    drop_na() %>%
    group_by(age,behavior) %>% 
    summarise(count = n()) %>% 
    mutate(perc = count*100/sum(count))
  
  # Generar el gráfico de barras mejorado
  ggplot(data_filtered, aes(x = age, fill = behavior, y=perc)) +
    geom_bar(position = "stack", stat = "identity", width = 0.7) +  # Ajuste del tamaño de las barras
    scale_fill_brewer(palette = "Set2") +  # Colores suaves y agradables
    labs(title = "Relación entre Edad y Comportamiento",
         x = "Edad de la Ardilla",
         y = "Porcentaje de Observaciones",
         fill = "Comportamiento") +
    geom_text(aes(label=round(perc,2)), size = 3, position = position_stack(vjust = 0.5))+
    coord_flip()+
    theme_minimal() +  # Fondo limpio
    theme(
      text = element_text(family = "Arial", size = 12),  # Fuente de texto más legible
      axis.title = element_text(size = 14),  # Títulos de los ejes en mayor tamaño
      axis.text = element_text(size = 12),  # Texto de los ejes en un tamaño adecuado
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Título más destacado
      legend.position = "bottom",  # Coloca la leyenda en la parte inferior
      legend.title = element_text(size = 12),  # Tamaño de título en la leyenda
      legend.text = element_text(size = 10)  # Tamaño de texto en la leyenda
    ) +
    guides(fill = guide_legend(title.position = "top", title.hjust = 0.5))  # Mejorar la presentación de la leyenda
}

# Usar la función para crear el gráfico
plot_age_vs_behavior(nyc_squirrels)

##-- Pregunta 2----

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
    stop("El color debe ser uno de los siguientes : Black, Cinnamon,Gray,Undefined")
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
    addProviderTiles("Esri.WorldTopoMap") %>%
    addLegend(title="Mapa de avistamientos de ardillas color:",
              pal=pal,values = ~colorSel)
  return(map)
}

#Generando mapas de cada opcion de color de ardilla
squirrelColorMap("Undefined",nyc_squirrels)
squirrelColorMap("Black",nyc_squirrels)
squirrelColorMap("cinnamon",nyc_squirrels)
squirrelColorMap("gray",nyc_squirrels)


## --Pregunta 3----
#' Comportamiento de las Ardillas a lo Largo de los Meses
#'
#' Esta función genera un heatmap para visualizar cómo cambia el comportamiento de las ardillas
#' a lo largo de los meses. La función toma un dataset con información sobre el comportamiento 
#' de las ardillas y su fecha de observación, y genera un gráfico que muestra la cantidad de 
#' observaciones para cada comportamiento en cada mes.
#'
#' @param data Un data frame que contiene los datos de las ardillas. Debe incluir una columna
#'             de fecha llamada 'date' y columnas para cada tipo de comportamiento de las ardillas 
#'             (por ejemplo, "running", "chasing", "climbing", "eating").
#' 
#' @return Un gráfico de tipo heatmap que muestra la cantidad de veces que cada comportamiento 
#'         ocurrió en cada mes.
#' 
#' @import ggplot2
#' @import dplyr
#' @import tidyr
#' @import viridis
#' @import lubridate
#' @export
plot_squirrel_behavior_by_month <- function(data) {
  #Separando la columna date en dia
  data[,day:= substr(date,3,4)]
  
  # Filtrar y reorganizar los datos
  behavior_data <- data %>%
    select(day, running, chasing, climbing, eating,foraging) %>%
    pivot_longer(cols = c("running", "chasing", "climbing", "eating","foraging"), 
                        names_to = "behavior", values_to = "engaged") %>%
    filter(engaged == TRUE) %>%
    count(day, behavior)  # Contar la cantidad de cada comportamiento por mes
  
  # Ver los primeros registros para asegurarse que la estructura es correcta
  head(behavior_data)
  
  # Crear el gráfico de tipo heatmap
  ggplot(behavior_data, aes(x = day, y = behavior, fill = n)) +
    geom_tile() +  # Crear los cuadros del heatmap
    scale_fill_viridis() +  # Usar la paleta de colores viridis
    labs(title = "Comportamiento de las Ardillas a lo Largo de los Meses",
         x = "Mes",
         y = "Comportamiento",
         fill = "Número de Observaciones") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      text = element_text(family = "Arial", size = 12),
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12)
    )
}
plot_squirrel_behavior_by_month(nyc_squirrels)