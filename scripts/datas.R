
# Gera datas --------------------------------------------------------------

gera_datas <- function(freq,
                       inicio,
                       fim) {
  # gera datas entre dois periodos com frequencia pré definida
  bys <- c("D" = "d",
           "M" = "m",
           "T" = "3 m",
           "S" = "6 m",
           "A" = "y")
  
  inicio_ano <- lubridate::year(inicio)
  inicio_mes <- lubridate::month(inicio)
  
  data <- NULL
  
  if(freq == "Q") {
    fim_mes <- lubridate::month(fim)
    fim_ano <- lubridate::year(fim)
    
    data1 <- seq.Date(from = as.Date(paste0(inicio_ano, "-", inicio_mes, "-01")),
                      to = as.Date(paste0(fim_ano, "-", fim_mes, "-01")),
                      by = "m")
    data15 <- seq.Date(from = as.Date(paste0(inicio_ano, "-", inicio_mes, "-15")),
                       to = as.Date(paste0(fim_ano, "-", fim_mes, "-15")),
                       by = "m")
    data <- sort(c(data1, data15))
    data <- data[data >= inicio & data <= fim]
  }
  
  if(is.null(data)) {
    
    data <- seq.Date(from = inicio,
                     to = fim,
                     by = bys[freq])
  }
  
  return(data)
}


# Próximo período -------------------------------------------------------

prox_periodo <- function(date, freq) {
  # obtem periodo seguinte com base em frequencia
  switch(freq,
         "M"   = lubridate::`%m+%`(date,months(1)),
         "Q"  = lubridate::`%m+%`(date,weeks(2)),
         "T" = lubridate::`%m+%`(date,months(3)),
         "S" = lubridate::`%m+%`(date,months(6)),
         "A"    = lubridate::`%m+%`(date,years(1)))
}

ante_periodo <- function(freq, date) {
  # obtem periodo anterior com base em frequencia
  switch(freq,
         "D"   = date - 1,
         "M"   = lubridate::`%m-%`(date,months(1)),
         "Q"  = lubridate::`%m-%`(date,weeks(2)),
         "T" = lubridate::`%m-%`(date,months(3)),
         "S" = lubridate::`%m-%`(date,months(6)),
         "A"    = lubridate::`%m-%`(date,years(1)))
}

# Lista datas ----------------------------------

lista_datas <- function(ms) {
  UseMethod("lista_datas")
}

lista_datas.default <- function(ms) {
  stop("Método válido somente para objeto macro_serie")
}

lista_datas.macro_serie <- function(ms) {
  freq <- ms$meta$frequencia
  inicio <- ms$meta$inicio
  fim <- ms$meta$fim
  
  serie <- ms$serie
  
  if(inherits(serie, "ts")) {
    data <- gera_datas(freq = freq,
                       inicio = inicio,
                       fim = fim)
  }
  
  if(inherits(serie, "zoo")) {
    data <- as.Date(index(serie))
  }
  
  return(data)
}
