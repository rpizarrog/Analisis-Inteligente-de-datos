# Funciones
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