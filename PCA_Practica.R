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
#   Total Public Debt --> Deuda Pública  
#   Retail Money Funds --> Flujo dinero en fondos de inversión retail

library(quantmod)
library(dplyr)


