# Vamos a llevar a cabo un análisis PCA del retorno del SP500 
# a partir de diferentes indicadores macroeconómicos
# (Todos los valores son variaciones interanuales tanto del índice 
# como de los indicadores, es decir; el YoY Change en %)
# (Descargados de la FRED a través de quantmod -> http://research.stlouisfed.org/fred2/)
# Estos son:
#   USSLIND --> Indicador económico lider de USA
#   ICSA --> Peticiones de desempleo
#   NAPM --> Índice Sector de fabricación
#   NMFBAI --> Índice de Actividad de Negocio
#   GDP --> Producto Interior Bruto
#   FPI--> Inversión privada en activos fijos
#   RRSFS --> Ventas minoristas
#   PERMITNSA --> Pemisos de construcción
#   GFDEBTN --> Deuda Pública  
#   WRMFSL --> Flujo dinero en fondos de inversión retail

# Queremos reducir el numero de dimensiones para quedarnos con los factores macroeconomicos
# que mas han influido historicamente en el pasado en el indice SP500

library(quantmod)
library(dplyr)

#Descargamos todos los indicadores - en el mismo orden en el quese han explicado previamente
indicators <- new.env()
tickers <- c('USSLIND', 'ICSA', 'NAPM', 'NMFBAI', 'GDP', 'FPI',
             'RRSFS', 'PERMITNSA', 'GFDEBTN', 'WRMFSL')

getSymbols(tickers, src='FRED', env = indicators, auto.assign = T)

#El primer registro vendrá dado porla fecha común mas pequeña
oldest.dates <- sapply(indicators, function(x){ 
                                      x <- rownames(as.data.frame(x))
                                      return(min(x)) })
first.date <- format(as.Date(max(oldest.dates)), '%Y')

#Funcion para pasar a formato interanual todos los tickers,
#filtramos a partir de la fecha común
#y devolvemos un dataframe para cada uno de los indicadores
# NOTA: Consideramos los datos hasta el 2014 inclusive ya que no todos los indicadores
#       tienen datos del 2015

parsingIndicator <- function(indicator){
    indicator <- to.yearly(indicator)[, 4]
    indicator <- indicator[paste(first.date, '::2014', sep='')]
    indicator <- data.frame(fecha = format(index(indicator), '%Y'), 
                            valor = coredata(indicator)[,1])
    return(indicator)
}

df.indicators <- Reduce(function(x, y) inner_join(x, y, by="fecha"),
                        lapply(indicators, parsingIndicator))

names(df.indicators) <- c('fecha', names(lapply(indicators, parsingIndicator)))

# Pasamos las fechas a rownames y dropeamos la columna fecha
rownames(df.indicators) <- df.indicators$fecha
df.indicators$fecha <- NULL

# Aniadimos a cada fila un '+' o '-' para saber
# el retorno del índice ese año, desde 1997 hasta 2014
returns.SP500 <- c('+', '+', '+', '-', '-', '-','+',
                   '+', '+', '+', '+', '-', '+', '+', '-', '+', '+', '+') 
rownames(df.indicators) <- paste(rownames(df.indicators), returns.SP500, sep = '')








######################## ANALISIS PCA ########################################

pr.out = prcomp(df.indicators ,scale=TRUE)

#Vamos a determinar la proporción de variabilidad explicada para determinar
#cuantas componentes vamos a coger
pr.var <- pr.out$sdev^2

# Calculamos la proporción PVE 
pve <- pr.var/sum(pr.var)

#Con el test del "codo", vemos que la pendiente decrece sustancialmente a partir
#de la tercera componente
plot(pve, type = 'b')

#De hecho, con las 3 primeras componentes explicamos el 99,3% de la variabilidad
#por lo tanto, éste será el número de componentes a elegir
cumsum(pve)

#Vemos ahora con un biplot las 2 primeras dimensiones y sus valores propios
biplot(pr.out,scale=0)

#Analizamos correlaciones entre componentes y valores propios
#Vemos una correlación de un 93% entre la CP3 y el indicador USSLIND
pr.out$rotation





















#############################################################################
# PRUEBA CON 3D


library(rgl)

pc <- princomp(df.indicators, cor=TRUE, scores=TRUE)

#Vemos el gráfico de "el brazo" para saber que componentes son
#las que recogen la variación de los datos
plot(pc,type="lines")

#Con un cumsum y los valores proporcionales, vemos el acumulativo
pve = pc$sdev/sum(pc$sdev)
plot(cumsum(pve), type = "b", ylim = c(0,1))

#En este caso, CP1 explica un 62% de la variación en el dato y CP2 el 24%,
#en total ambas acumulan en torno al 85/86%

# Plot con 2 componentes
biplot(pc)


# Plot con 3 Componentes
plot3d(pc$scores[,1:3])
text3d(pc$scores[,1:3],texts=rownames(df.indicators))
text3d(pc$loadings[,1:3], texts=rownames(pc$loadings), col="red")
coords <- NULL
for (i in 1:nrow(pc$loadings)) {
  coords <- rbind(coords, rbind(c(0,0,0),pc$loadings[i,1:3]))
}
lines3d(coords, col="red", lwd=4)


