# Librerías
library(ggplot2)
library(gtools)
library(mosaic)
library(dplyr)
library(cowplot)
# library(dslabs) No se utiliza pero tiene data set interesantes 14 oct 2022

options(scipen=999)
       

prueba_devolver_plot <- function(a, b) {
  
  a <- 1:5
  b <- 6:10
  datos <- data.frame(a=a, b=b)
  g <- ggplot(datos, aes(x=a, y=b)) + 
    geom_point()
  lista <- list(datos = datos, g = g)
  lista
  }
    


# 14 - Oct 2022
# Funciones para variables discretas

# Algunas funciones para variables discretas
# Función que devuelve tabla de distribución VE, Varianza y Desv. Std, recibe solo los casos
f.discretas.ve.v.sd <- function(casos) {
  #library(ggplot2)
  # Inicializando valores
  n <- sum(casos)
  x = 0:(length(casos) - 1)
  
  # Calculando columas
  prob_x <- casos / n
  acumulado <- cumsum(prob_x)
  x.prob_x <- x * prob_x
  
  
  # Valor esperado
  VE <- sum(x.prob_x)
  
  # Varianza y Desviación Std
  x_menos_VE <- x - VE
  x_menos_VE.CUAD <- x_menos_VE ^ 2
  x_menos_VE.CUAD.prob_x <- x_menos_VE.CUAD * prob_x
  varianza <- sum(x_menos_VE.CUAD.prob_x)
  desv.std <- sqrt(varianza)
  
  
  # Grafica
  #plot <- ggplot(mpg, aes(displ, hwy, colour = class)) +
  #        geom_point()
  
  tabla <- data.frame(x, casos, prob_x,
                      acumulado, x.prob_x,
                      VE,
                      x_menos_VE,
                      x_menos_VE.CUAD,
                      x_menos_VE.CUAD.prob_x
  )
  estadisticos <- list(tabla = tabla, x = x, N = n,
                       VE = VE, varianza = varianza, desv.std = desv.std)
  
  estadisticos
  
  
}


# Similar función a la anterior solo que recibe también los valores de la variable discreta
# Algunas veces los valores de la variable discreta cominezan por debajo de 0, o no inician en cero
f.discretas.ve.v.sd.val.disc <- function(discretas, casos) {
  #library(ggplot2)
  # Inicializando valores
  n <- sum(casos)
  x = discretas
  
  # Calculando columnas
  prob_x <- casos / n
  acumulado <- cumsum(prob_x)
  x.prob_x <- x * prob_x
  
  
  # Valor esperado
  VE <- sum(x.prob_x)
  
  # Varianza y Desviación Std
  x_menos_VE <- x - VE
  x_menos_VE.CUAD <- x_menos_VE ^ 2
  x_menos_VE.CUAD.prob_x <- x_menos_VE.CUAD * prob_x
  varianza <- sum(x_menos_VE.CUAD.prob_x)
  desv.std <- sqrt(varianza)
  
  
  # Grafica
  #plot <- ggplot(mpg, aes(displ, hwy, colour = class)) +
  #        geom_point()
  
  tabla <- data.frame(x, casos, prob_x,
                      acumulado, x.prob_x,
                      VE,
                      x_menos_VE,
                      x_menos_VE.CUAD,
                      x_menos_VE.CUAD.prob_x
  )
  estadisticos <- list(tabla = tabla, x = x, N = n,
                       VE = VE, varianza = varianza, desv.std = desv.std)
  
  estadisticos
  
  
}


# Funciones para distribuciones de probabilidad
# Funciones preparadas para DISTRIBUCION UNIFORME
plotunif <- function(x, min = 0, max = 1, lwd = 1, col = 1, ...) {
  
  # Rejilla de valores del eje X
  if (missing(x)) {
    x <- seq(min - 0.5, max + 0.5, 0.01)
  }
  
  if(max < min) {
    stop("'min' debe ser menor que 'max'")
  }
  
  plot(x, dunif(x, min = min, max = max),
       xlim = c(min - 0.25, max + 0.25), type = "l",
       lty = 0, ylab = "f(x)", ...) 
  segments(min, 1/(max - min), max, 1/(max - min), col = col, lwd = lwd)
  segments(min - 2, 0, min, 0, lwd = lwd, col = col)
  segments(max, 0, max + 2, 0, lwd = lwd, col = col)
  points(min, 1/(max - min), pch = 19, col = col)
  points(max, 1/(max - min), pch = 19, col = col)
  segments(min, 0, min, 1/(max - min), lty = 2, col = col, lwd = lwd)
  segments(max, 0, max, 1/(max - min), lty = 2, col = col, lwd = lwd)
  points(0, min, pch = 21, col = col, bg = "white")
  points(max, min, pch = 21, col = col, bg = "white")
}

unif_area <- function(min = 0, max = 1, lb, ub, col = 1,
                      acolor = "lightgray", ...) {
  x <- seq(min - 0.25 * max, max + 0.25 * max, 0.001) 
  
  if (missing(lb)) {
    lb <- min(x)
  }
  if (missing(ub)) {
    ub <- max(x)
  }
  if(max < min) {
    stop("'min' debe ser menor que 'max'")
  }
  
  x2 <- seq(lb, ub, length = 1000) 
  plot(x, dunif(x, min = min, max = max),
       xlim = c(min - 0.25 * max, max + 0.25 * max), type = "l",
       ylab = "f(x)", lty = 0, ...)   
  
  y <- dunif(x2, min = min, max = max)
  polygon(c(lb, x2, ub), c(0, y, 0), col = acolor, lty = 0)
  segments(min, 1/(max - min), max, 1/(max - min), lwd = 2, col = col)
  segments(min - 2 * max, 0, min, 0, lwd = 2, col = col)
  segments(max, 0, max + 2 * max, 0, lwd = 2, col = col)
  points(min, 1/(max - min), pch = 19, col = col)
  points(max, 1/(max - min), pch = 19, col = col)
  segments(min, 0, min, 1/(max - min), lty = 2, col = col, lwd = 2)
  segments(max, 0, max, 1/(max - min), lty = 2, col = col, lwd = 2)
  points(0, min, pch = 21, col = col, bg = "white")
  points(max, min, pch = 21, col = col, bg = "white")
}

# función para distribuciones
# 27 Oct 2021 Actualizado

# library(gtools)


# Función que devuelve la probabilidad conforme y de acuerdo a la 
# la fórmula de distribución binomial
# Recibe tres parámetros: 
# los valores de x, e valor de n y la probabilidad de éxito
# Devuelve las probabilidades para cada valor de la variable aleatoria discreta
f.prob.binom <- function (x,n,exito) {
  fracaso <- 1 - exito
  prob <- (factorial(n) / (factorial(x) * factorial(n-x)))  * ((exito^x) * (fracaso ^ (n-x)))
  prob
}

# 14 Oct 2022
# Función para tabla binomial. Devuelve tabla binomial
f.tabla_binom <- function(n, exito) {
  tabla <- data.frame(x = 0:n,
                      f.x = dbinom(x = 0:n, size = n, prob = exito),
                      F.x = pbinom(q = 0:n, size = n, prob = 0.80))
  tabla
}

# 14 Oct 2022
# Función para devuelve tabla binomial, VE Varianza y Desv. Std
# de una distribución binomial. 

f.binom.all <- function(n, exito){
  tabla <- data.frame(x = 0:n,
                      f.x = dbinom(x = 0:n, size = n, prob = exito),
                      F.x = pbinom(q = 0:n, size = n, prob = exito))
  tabla
  
  # Valor esperado
  VE <- n * exito # n * p
  
  # Varianza n * p * (n - 1)
  varianza <- n * exito * (1 - exito)
  desv.std <- sqrt(varianza)
  
  g.dens <- plotDist(dist = "binom", 
                    params = c(n, exito), 
                    xlim = c(-1, n+1), 
                    kind = "d",
                    xlab ="X's", 
                    ylab = "Probabilidad", 
                    main='Distribución Binomial',
                    sub = "Densidad")
  
  g.hist <- plotDist(dist = "binom", 
                    params = c(n, exito), 
                    xlim = c(-1, n+1), 
                    kind = "h", 
                    xlab ="X's", 
                    ylab = "Probabilidad", 
                    main='Distribución Binomial',
                    sub = "Histograma")
  
  g.acum <- plotDist(dist = "binom", 
                     params = c(n, exito), 
                     xlim = c(-1, n+1), 
                     kind = "c", 
                     xlab ="X's", 
                     ylab = "Prob Acumulada", 
                     main='Distribución Binomial',
                     sub = "")
  
  distribucion <- list(tabla = tabla, VE = VE, 
                       varianza = varianza, desv.std = desv.std, 
                       g.dens = g.dens, 
                       g.hist = g.hist,
                       g.acum = g.acum)

  
}

# Devolver histograma, histograma con densidad y acumulado
f.hist.dens.discreta <- function(datos) {
  library(ggplot2)
  
  g1 <- ggplot(data = datos) +
    geom_col(aes(x = x, y = f.x), fill='blue')
  
  casos <- NULL
  for(r in 1:nrow(datos)) {
    casos <- c(casos, c(rep(datos$x[r], round(datos[r,2] * 100))))
  }
  
  g2 <- ggplot() +
    geom_col(data = datos, aes(x = x, y = f.x)) +
    geom_density(aes(x = casos), color = 'red')
  
  g3 <- ggplot(data = datos) +
    geom_line(aes(x = x, y = F.x), color = 'red') +
    geom_point(aes(x = x, y = F.x), color = 'blue') 
  
  lista <- list(hist = g1, dens = g2, acum = g3)
  
  return(lista)
  
}


# Función de distribución de Poisson conforme a la Fórmula
f.prob.poisson <- function (media, x) {
  e <- exp(1)
  prob <- media^x * e^(-media) / factorial(x)
  prob
}

# Función de disrtibución hipergeométrica
# Recibe estos pa´rametros:
# N Total de elementos de la población
# n Elementos de la muestra o ensayos
# r número de elementos considerados como éxtio
# x Valores que puede tener la variabel aleatoria discreta
f.prob.hiper <- function (x, N, n, r) {
  numerador <- (factorial(r) / (factorial(x) * factorial(r-x))) * (factorial(N-r) / (factorial(n-x) * factorial((N-r)-(n-x)))) 
  denominador <- (factorial(N) / (factorial(n) * factorial(N-n)))
  
  prob <- numerador / denominador
  prob
}

# Función que devuelve el valor esperado de una distribución hipergeométrica
f.va.hiper <- function (n, r, N) {
  VE <- n * (r/N)
  VE
}

# Función que devuelve la varianza de una distribución hipergeométrica
f.varianza.hiper <- function(VE, n, r, N)  {
  varianza <- VE * (1 - r/N) * ((N-n) / (N-1))
  varianza
}

# Devuelve el valor de t para una distribución T Student
f.devolver.t <- function(media.muestra, media.pob, desv.muestra, n) {
  t <- (media.muestra - media.pob) / (desv.muestra / sqrt(n))
  t
}

f.t.student.std <- function(Z, V) {
  dens <- Z / (V / sqrt(V))
  dens
}

f.normal.dens <- function(desv, x, media) {
  numerador <- exp(1)^(-(x - media)^2 / (2 * desv^2))
  denominador <- desv * sqrt(2 * pi)
  prob <- numerador / denominador
  prob
  
  # Es lo mismo que dnorm(x = x, mean = media, sd = desv)
}

# Función para devolver el valor de z
f.devolver.z <- function(x, media, desv) {
  z <- (x - media) / desv
  z
}

f.devolver.z.prueba <- function(media.m, desv.p, media.p, n) {
  z <- (media.m - media.p) / (desv.p / sqrt(n))
  z
}

f.devolver.t.prueba <- function(media.m, desv.m, media.p, n) {
  t <- (media.m - media.p) / (desv.m / sqrt(n))
  t
}


# Función para devolver Z para Intervalo de Confianza
f.z.int.conf <- function (confianza) {
  alfa = 1 - confianza
  #alfa
  
  v.critico <- 1 - (alfa / 2)
  #v.critico
  
  z <- qnorm(v.critico)
  z
}


# Función para devolver el intervalo de confianza Z
# a cuatro posiciones decimales
f.intervalo.confianza <- function (media, desv, confianza, n) {
  li <- media - f.z.int.conf(confianza) * desv / sqrt(n) 
  ls <- media + f.z.int.conf(confianza) * desv / sqrt(n) 
  
  round(c(li, ls),4)
}
# Función para devolver el intervalo de confianza Z
# a cuatro posiciones decimales
f.intervalo.confianza.z <- function (media, desv, confianza, n) {
  li <- media - f.z.int.conf(confianza) * desv / sqrt(n) 
  ls <- media + f.z.int.conf(confianza) * desv / sqrt(n) 
  
  round(c(li, ls),4)
}



# Función para devolver t para Intervalo de Confianza
f.t.int.conf <- function (confianza, n) {
  alfa = 1 - confianza
  #alfa
  
  v.critico <- 1 - (alfa / 2)
  #v.critico
  
  t <- qt(v.critico, n-1)
  t
}

# Función para devolver el intervalo de confianza t
# a cuatro posiciones decimales
f.intervalo.confianza.t <- function (media, desv, confianza, n) {
  li <- media - f.t.int.conf(confianza, n) * desv / sqrt(n) 
  ls <- media + f.t.int.conf(confianza, n) * desv / sqrt(n) 
  
  round(c(li, ls),4)
}

# Función para graficar intervalo de confianza
# No funciona
f.graf.intervalo.confianza <- function (datos) {
  datos <- data.frame(valores = datos)
  g <- ggplot(data = datos) +
    geom_point(aes(x = f.devolver.z(x = valores, media = mean(valores), desv = sd(valores)), 
                   y = pnorm(f.devolver.z(x = valores, media = mean(valores), desv = sd(valores)))))
  g
}



# Devuelve la probabilidad de una variable discreta
# Recibe una tabla de distribución
# recibe el valor de la variable discreta
# Recibe el tipo: 
#   = 0; 
#   < 1; 
#   > 2; 
#   <= 3; 
#   >= 4 
f.prob.discr <- function(datos, discreta, tipo) {
  if (tipo == 0) { # == 
    salida <- filter(datos, x == discreta) %>%
      select (f.prob)    
  }
  if (tipo == 1) { # <
    salida <- filter(datos, x == discreta - 1) %>%
      select (f.acum) 
  }
  if (tipo == 2) { # >
    salida <- filter(datos, x == discreta) %>%
      select (f.acum)
    salida <- 1 - salida
  }
  if (tipo == 3) { # <=
    salida <- filter(datos, x == discreta) %>%
      select (f.acum)
  }
  if (tipo == 4) { # >=
    salida <- filter(datos, x == discreta - 1) %>%
      select (f.acum)
    salida <- 1 - salida
  }
  salida
}