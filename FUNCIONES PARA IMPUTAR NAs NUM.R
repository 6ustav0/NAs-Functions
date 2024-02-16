
# FUNCIONES PARA IMPUTAR NAs NUMÉRICAS

minmax <- function(x, y) {
  resul1=x-y
  resul2=x+y
  resultados <- list(min=resul1,
                     max=resul2)
  return(resultados)
}

arreglo_variable <- function(f) {
  mn_sd <- minmax(x=round(mean(f, na.rm = TRUE), 0), y=round(sd(f, na.rm = TRUE), 0))
  mn_sd
  aleat = 0
  for (i in 1:3072) {
    if (is.na(f[i])) { 
      aleat <- sample(mn_sd$min:mn_sd$max, 1)
      f[i] <- case_when(aleat < 1 ~ 1,
                        aleat > mn_sd$max ~ mn_sd$max,
                        TRUE ~ aleat)
    } else {next}
  }
  return(f)
}
