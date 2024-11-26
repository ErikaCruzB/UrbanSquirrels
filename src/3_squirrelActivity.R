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
