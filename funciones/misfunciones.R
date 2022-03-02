# Funciones varias
# Función que determina tabla de un archivo
# data.frame con dos variables numéricas
f_tabla.rls <- function(datos) {
  library(dplyr)
  x <- datos[1,]
  y <- datos[2,]
  n <- nrow(datos)
  datos <- datos %>%
    mutate(media.x = rep(mean(x), n),
           media.y = rep(mean(y), n))
  # datos
  datos <- datos %>%
    mutate(x.menos.media.x = x-media.x,
           y.menos.media.y = y - media.y, x.menos.media.x.por.y.menos.media.y = (x-media.x)*(y-media.y), x.menos.media.x.cuad = (x-media.x)^2)
  
  datos <- rbind(datos, as.numeric(apply(X = datos, MARGIN = 2, FUN = sum)))
  
  datos
  
  datos  
  
  
}

# Devuelve los coeficiente a y b de una 
# regresión lineal simple
f.coef.a.b <- function(tabla) {
  b = tabla[nrow(tabla), 7] / tabla[nrow(tabla), 8]
  a = mean(mean(tabla$y[1:nrow(tabla)-1])) - mean(mean(tabla$x[1:nrow(tabla)-1])) * b
  c(a, b)
}