<<<<<<<< HEAD:src/Original_scripts/SquirrelBehavior.R
#Autores: 
#Descripción:
#Fecha: 
  
#Datos:   
readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-29/nyc_squirrels.csv")

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
plot_age_vs_behavior <- function(data, behavior = c("running", "chasing", "climbing", "eating")) {
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  
  # Convertir la columna 'age' en un factor para asegurar que ggplot lo trate como categorías
  data$age <- as.character(data$age)
  data$age[!data$age %in% c('Juvenile', 'Adult')] <- NA
  data$age <- factor(data$age, levels = c('Juvenile', 'Adult'))
  
  # Filtrar los datos para eliminar NA en 'age' y 'behavior'
  data_filtered <- data %>%
    dplyr::select(age, all_of(behavior)) %>%
    tidyr::pivot_longer(cols = all_of(behavior), names_to = "behavior", values_to = "engaged") %>%
    dplyr::filter(engaged == TRUE) %>%
    dplyr::filter(!is.na(age)) %>% 
    tidyr::drop_na() 
  
  # Generar el gráfico de barras mejorado
  ggplot(data_filtered, aes(x = age, fill = behavior)) +
    geom_bar(position = "dodge", stat = "count", width = 0.7) +  # Ajuste del tamaño de las barras
    scale_fill_brewer(palette = "Set2") +  # Colores suaves y agradables
    labs(title = "Relación entre Edad y Comportamiento",
         x = "Edad de la Ardilla",
         y = "Número de Ardillas",
         fill = "Comportamiento") +
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

# Cargar los datos
library(readr)
nyc_squirrels <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-29/nyc_squirrels.csv")

# Usar la función para crear el gráfico
plot_age_vs_behavior(nyc_squirrels)



## --Pregunta 3----
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(viridis)
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
  # Asegurarse de que 'date' esté en formato Date
  data$date <- as.Date(data$date)
  
  # Extraer el mes de la fecha
  data$month <- month(data$date, label = TRUE)  # Obtener el mes en formato abreviado
  
  # Filtrar y reorganizar los datos
  behavior_data <- data %>%
    dplyr::select(month, running, chasing, climbing, eating) %>%
    tidyr::pivot_longer(cols = c("running", "chasing", "climbing", "eating"), 
                        names_to = "behavior", values_to = "engaged") %>%
    dplyr::filter(engaged == TRUE) %>%
    dplyr::count(month, behavior)  # Contar la cantidad de cada comportamiento por mes
  
  # Ver los primeros registros para asegurarse que la estructura es correcta
  head(behavior_data)
  
  # Crear el gráfico de tipo heatmap
  ggplot(behavior_data, aes(x = month, y = behavior, fill = n)) +
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


#R version 4.4.0 (2024-04-24 ucrt)
#Platform: x86_64-w64-mingw32/x64
#Running under: Windows 11 x64 (build 22631)

#Matrix products: default


#locale:
 # [1] LC_COLLATE=Spanish_Mexico.utf8  LC_CTYPE=Spanish_Mexico.utf8    LC_MONETARY=Spanish_Mexico.utf8 LC_NUMERIC=C                   
#[5] LC_TIME=Spanish_Mexico.utf8    

#time zone: America/Mexico_City
#tzcode source: internal

#attached base packages:
 # [1] stats     graphics  grDevices utils     datasets  methods   base     

#other attached packages:
 # [1] viridis_0.6.5     viridisLite_0.4.2 lubridate_1.9.3   tidyr_1.3.1       dplyr_1.1.4       ggplot2_3.5.1     readr_2.1.5      

#loaded via a namespace (and not attached):
 # [1] crayon_1.5.3       vctrs_0.6.5        cli_3.6.3          rlang_1.1.4        purrr_1.0.2        generics_0.1.3     labeling_0.4.3    
#[8] glue_1.7.0         bit_4.5.0          colorspace_2.1-0   gridExtra_2.3      hms_1.1.3          scales_1.3.0       fansi_1.0.6       
#[15] grid_4.4.0         munsell_0.5.1      tibble_3.2.1       tzdb_0.4.0         lifecycle_1.0.4    compiler_4.4.0     sessioninfo_1.2.2 
#[22] RColorBrewer_1.1-3 timechange_0.3.0   pkgconfig_2.0.3    rstudioapi_0.17.0  farver_2.1.2       R6_2.5.1           tidyselect_1.2.1  
#[29] utf8_1.2.4         vroom_1.6.5        pillar_1.9.0       curl_5.2.3         parallel_4.4.0     magrittr_2.0.3     tools_4.4.0       
#[36] withr_3.0.1        bit64_4.5.2        gtable_0.3.5      



========
#Autoras: Erika Viridiana Cruz Bonilla y Amairani Cancino Bello
#Descripción: 
#Fecha: 26/11/2024

#Descarga de Datos:   
readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-29/nyc_squirrels.csv")

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
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  
  # Convertir la columna 'age' en un factor para asegurar que ggplot lo trate como categorías
  data$age <- as.character(data$age)
  data$age[!data$age %in% c('Juvenile', 'Adult')] <- NA
  data$age <- factor(data$age, levels = c('Juvenile', 'Adult'))
  
  # Filtrar los datos para eliminar NA en 'age' y 'behavior'
  data_filtered <- data %>%
    dplyr::select(age, all_of(behavior)) %>%
    tidyr::pivot_longer(cols = all_of(behavior), names_to = "behavior", values_to = "engaged") %>%
    dplyr::filter(engaged == TRUE) %>%
    dplyr::filter(!is.na(age)) %>% 
    tidyr::drop_na() 
  
  # Generar el gráfico de barras mejorado
  ggplot(data_filtered, aes(x = age, fill = behavior)) +
    geom_bar(position = "dodge", stat = "count", width = 0.7) +  # Ajuste del tamaño de las barras
    scale_fill_brewer(palette = "Set2") +  # Colores suaves y agradables
    labs(title = "Relación entre Edad y Comportamiento",
         x = "Edad de la Ardilla",
         y = "Número de Ardillas",
         fill = "Comportamiento") +
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

# Cargar los datos
library(readr)
nyc_squirrels <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-29/nyc_squirrels.csv")

# Usar la función para crear el gráfico
plot_age_vs_behavior(nyc_squirrels)



##-- Pregunta 2----
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



##-- pregunta 3---- 
# Paquetes necesarios
packages <- c("data.table", "ComplexHeatmap")

# Instalar paquetes no instalados
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Cargar paquetes
invisible(lapply(packages, library, character.only = TRUE))

#Descargar y cargar datos de ardillas
nyc_squirrels <- fread("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-29/nyc_squirrels.csv")

#Separando la columna date en dia
nyc_squirrels[,day:= as.numeric(substr(date,3,4))]

#Contabilizando cuantos avistamientos hay por mes

nyc_squirrels[,.N,by = .(day,)]

tsquirrels<-melt(nyc_squirrels,id.vars = c("unique_squirrel_id","day"), measure.vars = 16:20)
tsquirrels<-tsquirrels[value == TRUE,]

actTable<-tsquirrels[,.N,by = .(day,variable)]

actTable<-dcast(actTable, variable~day, value.var = "N", fill = 0)
actTable<-as.data.frame(actTable)
row.names(actTable)<-actTable$variable
actTable<-actTable[,-1]
actTable<-as.matrix(actTable) 

Heatmap(actTable,show_column_dend = FALSE,show_row_dend = FALSE,cluster_columns = F)



#sessionInfo()
#R version 4.4.0 (2024-04-24 ucrt)
#Platform: x86_64-w64-mingw32/x64
#Running under: Windows 11 x64 (build 22631)

#Matrix products: default


#locale:
# [1] LC_COLLATE=Spanish_Mexico.utf8  LC_CTYPE=Spanish_Mexico.utf8    LC_MONETARY=Spanish_Mexico.utf8 LC_NUMERIC=C                   
#[5] LC_TIME=Spanish_Mexico.utf8    

#time zone: America/Mexico_City
#tzcode source: internal

#attached base packages:
# [1] stats     graphics  grDevices utils     datasets  methods   base     

#other attached packages:
# [1] viridis_0.6.5     viridisLite_0.4.2 lubridate_1.9.3   tidyr_1.3.1       dplyr_1.1.4       ggplot2_3.5.1     readr_2.1.5      

#loaded via a namespace (and not attached):
# [1] crayon_1.5.3       vctrs_0.6.5        cli_3.6.3          rlang_1.1.4        purrr_1.0.2        generics_0.1.3     labeling_0.4.3    
#[8] glue_1.7.0         bit_4.5.0          colorspace_2.1-0   gridExtra_2.3      hms_1.1.3          scales_1.3.0       fansi_1.0.6       
#[15] grid_4.4.0         munsell_0.5.1      tibble_3.2.1       tzdb_0.4.0         lifecycle_1.0.4    compiler_4.4.0     sessioninfo_1.2.2 
#[22] RColorBrewer_1.1-3 timechange_0.3.0   pkgconfig_2.0.3    rstudioapi_0.17.0  farver_2.1.2       R6_2.5.1           tidyselect_1.2.1  
#[29] utf8_1.2.4         vroom_1.6.5        pillar_1.9.0       curl_5.2.3         parallel_4.4.0     magrittr_2.0.3     tools_4.4.0       
#[36] withr_3.0.1        bit64_4.5.2        gtable_0.3.5      


>>>>>>>> 747d3745cc09ce95a122274da460a4b6db1db894:src/squirrelScript.R
