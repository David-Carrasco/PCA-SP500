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

#Cogeremos en este caso, 3 componentes que engloban un PVE de 89,4%, 
#aunque con 2 componentes ya tenemos un PVE de 82%
cumsum(pve) * 100

#Analizamos correlaciones entre componentes y valores propios
pr.out$rotation

#La única correlación significativa entre los valores propios y las 3 componentes
#es una alta correlación inversa entre el indicador PERMITNSA y la PC3 (-0.73406267)
#con lo cual, cuanto menor sea el valor de PC3 del año en concreto, mayor será su indicador PERMITNSA
#y viceversa en un relación de un 73%

#Vemos ahora con un biplot las 2 primeras componentes y sus valores propios
biplot(pr.out, scale = 0)

#Podemos observar una alta concentración de retornos positivos 
#para años con PC1 < 0 y -2 < PC2 < 2
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

#Por tanto, parece que lo que tiene en común son unos datos posivitos macroeconómicos
#que pudieron favorecer los retornos anuales del índice SP500
#Habría que seguir confirmando en un futuro que años con retornos positivos en bolsa,
#la mayoría de ellos, se sitúan en esa concentración de los datos del PCA que hemos estudiado



