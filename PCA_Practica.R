# Vamos a llevar a cabo un análisis PCA del retorno del SP500 
# a partir de diferentes indicadores macroeconómicos
# (Todos los valores son variaciones interanuales tanto del índice 
# como de los indicadores, es decir; el YoY Change)
# (Descargados de la FRED a través de quantmod -> http://research.stlouisfed.org/fred2/)
# Estos son:
#   LEI --> Indicador económico lider de USA
#   Initial Claims --> Peticiones de desempleo
#   ISM Manufacturing: PMI Composite Index --> Índice Sector de fabricación
#   ISM non Manufacturing --> Índice de Actividad de Negocio
#   GDP --> Producto Interior Bruto
#   Fixed Private Investment --> Inversión privada en activos fijos
#   Real Retail and Food Services Sales --> Ventas minoristas
#   New Private Housing Units Authorized by Building Permits --> Pemisos de construcción
#   Federal Total Public Debt --> Deuda Pública  
#   Retail Money Funds --> Flujo dinero en fondos de inversión retail

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

######################## ANALISIS PCA ########################################



