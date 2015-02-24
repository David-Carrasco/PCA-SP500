# Vamos a llevar a cabo un análisis PCA del retorno del SP500 
# a partir de diferentes indicadores macroeconómicos
#
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

# El objetivo es reducir el numero de dimensiones para quedarnos con los factores macroeconomicos
# que mas han influido historicamente en el pasado en el indice SP500

library(quantmod)
library(dplyr)

#Descargamos todos los indicadores - en el mismo orden en el quese han explicado previamente
indicators <- new.env()
tickers <- c('USSLIND', 'ICSA', 'NAPM', 'NMFBAI', 'GDP', 'FPI',
             'RRSFS', 'PERMITNSA', 'GFDEBTN', 'WRMFSL')

getSymbols(tickers, src='FRED', env = indicators, auto.assign = T)

#El primer registro vendrá dado por la fecha común mas pequeña
oldest.dates <- sapply(indicators, function(x){ 
                                      x <- rownames(as.data.frame(x))
                                      return(min(x)) })
first.date <- format(as.Date(max(oldest.dates)), '%Y')


# Aquellos que no son acumulativos, quedarán tal cual vienen:
# NMFBAI
# NAPM
# USSLIND

# Ahora, pasaremos todos los valores a retornos interanuales a traves de la funcion parsingIndicator.

# Funcion para pasar a formato interanual todos los tickers,
# filtrar a partir de la fecha común como punto de partida del dataset
# y devuelve un vector con el indicador
# NOTA: Consideramos los datos hasta el 2014 inclusive ya que no todos los indicadores
#       tienen datos del 2015

parsingIndicator <- function(indicator){
  
  year.indicator <- to.yearly(indicator)[, 4]
  
  #Si son indicadores acumulativos, sacamos el YoY change en %
  if (!names(indicator) %in% c('NMFBAI', 'NAPM', 'USSLIND')){
    year.indicator <- round(Delt(year.indicator) * 100, 3)
  }
  
  year.indicator <- year.indicator[paste(first.date, '::2014', sep='')]
  year.indicator <- coredata(year.indicator)[,1]
  return(year.indicator)
}

#Dataframe con todos los indicadores agrupados
df.indicators <- as.data.frame(row.names = c(first.date:2014),
                               do.call(cbind, lapply(indicators, parsingIndicator)))

#El índice ICSA cuenta las peticiones de desempleo, con lo cual, invertimos el signo
#para que sea positivo si decrece el desempleo y viceversa
df.indicators$ICSA <- df.indicators$ICSA * -1

# Aniadimos a cada fila un '+' o '-' para saber
# el retorno del índice ese año, desde 1997 hasta 2014
returns.SP500 <- c('+', '+', '+', '-', '-', '-','+',
                   '+', '+', '+', '+', '-', '+', '+', '+', '+', '+', '+') 
rownames(df.indicators) <- paste(rownames(df.indicators), returns.SP500, sep = '')

######################## ANALISIS PCA ########################################

pr.out = prcomp(df.indicators, scale=TRUE)

#Vamos a determinar la proporción de variabilidad explicada para determinar
#cuantas componentes vamos a coger
pr.var <- pr.out$sdev^2

# Calculamos la proporción PVE 
pve <- pr.var/sum(pr.var)

#Con el test del "codo", vemos que la pendiente decrece sustancialmente a partir
#de la segunda/tercera componente
plot(pve, type = 'b')

#Podríamos coger 2 componentes con un PVE de 82% o bien,
#coger 3 componentes con un PVE de 89,4%
cumsum(pve) * 100

#Vemos ahora con un biplot las 2 primeras dimensiones y sus valores propios
biplot(pr.out,scale=0)

#Analizamos correlaciones entre componentes y valores propios
pr.out$rotation



