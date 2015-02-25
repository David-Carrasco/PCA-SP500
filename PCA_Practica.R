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
#   FPI --> Inversión privada en activos fijos
#   RRSFS --> Ventas minoristas
#   PERMITNSA --> Pemisos de construcción
#   GFDEBTN --> Deuda Pública  
#   WRMFSL --> Flujo dinero en fondos de inversión retail

# El objetivo es reducir el numero de dimensiones para analizar como 
#los factores macroeconomicos han podido influir históricamente en el pasado en el indice SP500

library(quantmod)
library(dplyr)

#Descargamos todos los indicadores
indicators <- new.env()
tickers <- c('USSLIND', 'ICSA', 'NAPM', 'NMFBAI', 'GDP', 'FPI',
             'RRSFS', 'PERMITNSA', 'GFDEBTN', 'WRMFSL')

getSymbols(tickers, src='FRED', env = indicators, auto.assign = T)

#La primera observación de los datos vendrá dada por la fecha común mas pequeña
oldest.dates <- sapply(indicators, function(x){ 
                                      x <- rownames(as.data.frame(x))
                                      return(min(x)) })
first.date <- format(as.Date(max(oldest.dates)), '%Y')

# Aquellos que no son acumulativos, quedarán tal cual vienen:
# NMFBAI
# NAPM
# USSLIND

# Ahora, pasaremos todos los valores a retornos interanuales a traves de la funcion parsingIndicator.

# Función para pasar a formato interanual todos los tickers,
# filtrar a partir de la fecha común como punto de partida del dataset
# y devolver un vector con el indicador modificado
# NOTA: Consideramos los datos hasta el 2014 inclusive ya que no todos los indicadores
#       tienen datos del 2015 aún

parsingIndicator <- function(indicator){
  
  year.indicator <- to.yearly(indicator)[, 4]
  
  #En los indicadores que son acumulativos, sacaremos el YoY change en %
  #En aquellos que no, se dejarán tal cual son descargos con quantmod
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
#de la tercera componente
plot(pve, type = 'b')

#Cogeremos en este caso, 2 componentes que engloban un PVE de 82%, 
#aunque con 3 componentes tendríamos un PVE de 89.4%
cumsum(pve) * 100

#Analizamos correlaciones entre componentes y valores propios
pr.out$rotation

#No hay correlaciones importantes entre las dimensiones y las 2 primeras componentes principales

#Vemos ahora con un biplot las 2 primeras componentes y sus valores propios
biplot(pr.out, scale = 0)

#Podemos observar una alta concentración de retornos positivos 
#para años localizados en PC1 < 0 y -2 < PC2 < 2
#Por tanto, vamos a estudiar los indicadores macroeconomicos de esos años:
filter_df <- rownames(subset(as.data.frame(pr.out$x), PC1 < 0 & PC2 > -2 & PC2 < 2))

#Analizamos cada uno de los indicadores de la selección anterior
df.positive.returns <- df.indicators[filter_df, ]
summary(df.positive.returns)

#Conclusiones:
#El GDP crece en todos esos años, mínimo a un 3,467% anual

#El número de licencias de construcción no es altamente relevante
#ya que tiene tanto valores positivos como negativos,
#aunque la mediana indica un sesgo posivitivo durante esos años --> incremento de un 7.83% anual

#El número de peticiones de desempleo en todos esos años, decrece al menos un 3,15% anual

#El índice de Actividad de Negocio en todos esos años es como mínimo 54.3, es decir;
#está por encima de 50 que es el punto límite entre expansión o recesión
#Ref --> http://www.investopedia.com/university/releases/servicereport.asp

#El índice Sector de fabricación en todos esos años es como mínimo 50.4, es decir;
#está por encima de 50 que es el punto límite entre expansión o recesión
#Ref --> http://www.investopedia.com/terms/p/pmi.asp

#La deuda pública durante esos años, ha crecido como mínimo al 2.721% anual

#La inversión privada en activos fijos durante esos años, ha sido como mínimo de un 5.82% anual

#La inversión en fondos de inversión retail varía esos años entre un -10% y 15% anual y
#su media es de un -2% con lo cual, no aporta información relevante

#Las ventas minoristas durante esos años, han crecido mínimo al 1.38% anual

#El Indicador económico lider de USA durante esos años, se ha situado como mínimo
#en un 1.18%, es decir; está por encima del 0% que es el punto límite entre expansión y peligro de recesión
#Ref: http://www.investopedia.com/terms/c/cili.asp

#Por tanto, parece que lo que tienen en común estos años son unos datos posivitos macroeconómicos
#que pudieron favorecer  esos retornos positivos.
#Habría que seguir confirmando en un futuro que la mayoría de los años con retornos positivos en bolsa,
#se sitúan en esa concentración de datos que hemos obtenido a través del PCA

