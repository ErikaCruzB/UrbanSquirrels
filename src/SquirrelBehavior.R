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



## --Pregunta 2----
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

