# SquirrelSearchNYC
**Autoras:**

  - Amairani Cancino Bello
  - Erika Viridiana Cruz Bonilla

**Resumen**

El código presentado aquí analiza datos de avistamientos de ardillas en Central Park, Nueva York, recopilados por el proyecto The Squirrel Census en octubre de 2018. Se emplearon funciones automatizadas para generar gráficos de barras, mapas interactivos y heatmaps, garantizando análisis reproducibles.

Se exploraron aspectos clave como el comportamiento de las ardillas frente a humanos, la distribución por color de pelaje y las variaciones en sus comportamientos a lo largo del mes.

Con estos datos, se encontró que las ardillas juveniles y adultas mostraron comportamientos similares hacia los humanos, siendo la mayoría indiferente y menos del 10% acercándose a ellos. En términos de color de pelaje, las ardillas grises fueron las más abundantes y ampliamente distribuidas, seguidas por las cafés, mientras que las negras fueron menos comunes y con avistamientos concentrados en áreas específicas. También hubo un pequeño grupo de ardillas sin color definido. A lo largo del mes, la actividad más observada en las ardillas fue la búsqueda de comida, con un pico significativo el 13 de octubre. En contraste, la persecución fue la actividad menos frecuente.



Martes 26 de noviembre- Primera revisión. (3 PM)

Miercoles 27 de noviembre - tener listo el script.

Viernes 29 de noviembre - Revision 2. 

Lunes 1 de diciembre- Revisión 3 (quarto y presentación)

Miercoles 3 de diciembre - Quarto y la presentación.

Paso 1: Seleccionar uno de los Datasets precargados en R o los que son propios de los paquetes.

Dataset elegido: nyc_squirrels <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-29/nyc_squirrels.csv")

Paso 2: Analiza los datos y genera 3 preguntas que puedas contestar con las variables que tienes.
Preguntas propuestas: 

**Pregunta 1** *(amairani)*: Relación de la edad con respecto al comportamiento con los humanos. Tipo de grafico a generar: Grafico de barras o heatmap. (https://r-charts.com/es/ranking/grafico-barras-ggplot2/)

**Pregunta 2** *(erika)*: Relación del color con respecto al area donde han sido vistas. Tipo de grafico: Mapa de NY con puntos de colores. (https://r-charts.com/es/espacial/mapas-ggplot2/) (https://rpubs.com/cristobalrstudio/840002)

**Pregunta 3** *(amairani y erika)*: Cómo se comportan las ardillas a lo largo de los meses. Su comportamiento cambia de acuerdo a la epoca del año? Tipo de grafico: Heatmap (?) (https://jokergoo.github.io/ComplexHeatmap-reference/book/)


Paso 3: Genera un script completo que nos conteste las preguntas realizadas y nos de graficas de cada uno.

NOTA: No olvides agregar la programacion defensiva y un poco de todo lo visto en clase (Manipulacion de datos, funciones, BUENAS PRACTICAS, ggplot2 o Complexheatmap).
NOTA: No olvides que es muy importante documentar todo.
*(no olvidar poner la documentación roxygen)*

Paso 4: Entregables

Crear un reporte o un libro en Rmarkdown o Quarto (entregarlo el 5 de Diciembre antes de las 11 pm hora Ciudad de Mexico). *(amairani y erika)*

Genera una exposicion breve (empleando Xaringan o Quarto en R) de 15 min en total por equipo (practica el tiempo para que sea de provecho).

Sube todo a Github, el repositorio debe ser publico y debe contener tu reporte y presentacion. NO olvides poner tu nombre y la de tu equipo en el README.

Exposicion el 6 de diciembre
Instrucciones completas: https://eveliacoss.github.io/Defensiva_ggplot2024/Parte5.html
