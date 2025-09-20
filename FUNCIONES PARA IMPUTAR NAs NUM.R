

minmax <- function(x, y) {
  resul1 = x - y
  resul2 = x + y
  resultados <- list(min = resul1,
                     max = resul2)
  return(resultados)
}

# La función arreglo_variable se vectoriza.
arreglo_variable_opt <- function(f) {
  # Calculamos la media y la desviación estándar una sola vez.
  media <- round(mean(f, na.rm = TRUE), 0)
  desviacion_estandar <- round(sd(f, na.rm = TRUE), 0)
  
  mn_sd <- minmax(x = media, y = desviacion_estandar)
  
  # Generamos un vector de valores aleatorios del mismo tamaño que f.
  # El bucle for desaparece.
  aleatorios <- sample(mn_sd$min:mn_sd$max, size = length(f), replace = TRUE)
  
  # Imputamos solo donde hay NAs, usando una máscara lógica.
  # Y aplicamos las condiciones de forma vectorizada.
  f_imputado <- ifelse(is.na(f),
                       case_when(
                         aleatorios < 1 ~ 1,
                         aleatorios > mn_sd$max ~ mn_sd$max,
                         TRUE ~ aleatorios
                       ),
                       f)
  
  return(f_imputado)
}

# 1
