# Análisis de regresión con datos de precios de Carros
# Objetivo: Realizar una evaluación de modelos de regresión con datos de COVID del año 2022

# 

setwd("~/Semestre Agosto Diciembre 2022/Analisis Inteligente de Datos 9AC 1300/datos")

# Librerías
library(readr)
library(PerformanceAnalytics) # Para correlaciones gráficas
library(dplyr)
library(knitr) # Para datos tabulares
library(ggplot2) # Para visualizar
library(plotly) # Para visualizar
library(caret)  # Para particionar
library(Metrics) # Para determinar rmse

library(rpart) # Para árbol
library(rpart.plot) # Para árbol

library(randomForest) # Para random forest
library(caret) # Para hacer divisiones o particiones
library(reshape)    # Para renombrar columnas


datos <-  read.csv("price car/CarPrice_Assignment.csv", 
                   fileEncoding = "UTF-8", 
                   stringsAsFactors = TRUE)

# Descripción de los datos

str(datos)

# Variables 
cat(colnames(datos), sep = ",")

# Cuáles variables son numéricas
# Función que recibe un data.frame y 
# devuelve las variables numéricas
f_variables_numericas  <- function (datos) {
  numericas <- NULL
  i <- 1
  cols <- ncol(datos)
  
  # print(cols)
  for (c in 1:cols) {
    if (is.numeric(datos[,c])){
      numericas[i] <- paste0("",colnames(datos[c]),"")
      i <- i + 1
    }
      
  }
  numericas
}

# Cuáles variables estaría correlacionadas

# cor(x = , y = )
# Fuente: Enginesize, curbweight,horsepower,cardwidth y carlenght

vars_numericas <- f_variables_numericas(datos)
vars_numericas

correla <- data.frame(cor(datos[,vars_numericas]))
correla

correla <- correla %>%
    arrange(desc(price))

correla


chart.Correlation(datos[,vars_numericas])



# Limpiar datos
# Seleccionar solo las columnas numéricas
datos.numericas <- select(datos, vars_numericas)

datos.numericas

# Datos de Entrenamiento y Datos de Validación

n <- nrow(datos)
set.seed(2022) # Semilla
entrena <- createDataPartition(y = datos$price, p = 0.70, list = FALSE, times = 1)
# Datos entrenamiento
datos.entrenamiento <- datos.numericas[entrena, ]  # [renglones, columna]
# Datos validación
datos.validacion <- datos.numericas[-entrena, ]


# Modelo de regresión lineal múltiple para observar variables de importancia
modelo_rm <- lm(formula = price ~ symboling +  wheelbase +
                  carlength + carwidth + carheight + curbweight +
                  enginesize + boreratio + stroke + compressionratio +
                  horsepower + peakrpm + citympg + highwaympg , 
                data = datos.entrenamiento)

summary(modelo_rm)

# Se observa las variables que tiene mayor significancia sobre el precio 
# por encima del 95% de confianza
# enginesize, stroke, compressionratio, horsepower, 
# peakrpm, citympg          

# se reconstruye el modelo con esas variables

modelo_rm <- lm(formula = price ~ enginesize + stroke + 
                  compressionratio + horsepower + peakrpm + citympg , 
                data = datos.entrenamiento)

summary(modelo_rm)

# El modelo tiene un valor de 0.8394 como estadístico Adjusted R-squared que
# significa cuanto representa as variables independientes a la variable 
# dependiente precio (price).


# Predicciones
predicciones_rm <- predict(object = modelo_rm, newdata = datos.validacion)
predicciones_rm

# Generar matriz de comparación
comparaciones <- data.frame(datos.validacion, predicciones_rm)
comparaciones

# Evaluar RMSE
rmse_rm <- rmse(comparaciones$price, comparaciones$predicciones_rm)
rmse_rm



# Arboles de regresión

modelo_ar <- rpart(data = datos.entrenamiento,
                   formula = price ~ enginesize + stroke + 
                     compressionratio + horsepower + peakrpm + citympg )
modelo_ar

# Importancia de las variables
summary(modelo_ar)

# Representar visualmente el modelo
rpart.plot(modelo_ar)

# Predicciones

predicciones_ar <- predict(object = modelo_ar, 
                        newdata = datos.validacion)

# Generar matriz de comparación
comparaciones <- data.frame(datos.validacion, predicciones_ar)
comparaciones

# Evaluar RMSE
rmse_ar <- rmse(comparaciones$price, comparaciones$predicciones_ar)
rmse_ar



# Modelo Random Forest

modelo_rf <- randomForest(x = datos.entrenamiento[,c('enginesize', 'stroke',
                                                  'compressionratio',
                                                  'horsepower','peakrpm',
                                                  'citympg')], 
                          y = datos.entrenamiento[,'price'], 
                          #xtest = datos.validacion[,c('enginesize', 'stroke',
                          #                         'compressionratio',
                          #                         'horsepower','peakrpm',
                          #                         'citympg')], 
                          #ytest = datos.validacion[,'price'], 
                          importance = TRUE, 
                          keep.forest = TRUE, 
                          ntree=20)
modelo_rf

# Importancia de las variables
modelo_rf$importance

# Predicciones
predicciones_rf <- predict(object = modelo_rf, newdata = datos.validacion)
predicciones_rf

# Comparaciones
comparaciones <- data.frame(datos.validacion, predicciones_rf)
comparaciones

# Evaluar RMSE
rmse_rf <- rmse(comparaciones$price, comparaciones$predicciones_rf)
rmse_rf

comparaciones <- data.frame(datos.validacion[,c('enginesize', 'stroke',
                                                'compressionratio',
                                                'horsepower','peakrpm',
                                                'citympg')], predicciones_rm, 
                            predicciones_ar, predicciones_rf)
rmse <- data.frame(rm = rmse_rm, ar = rmse_ar, rf = rmse_rf)



