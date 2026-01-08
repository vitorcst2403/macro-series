
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
  
  inicio = as.Date(inicio)
  fim = as.Date(fim)
  
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


# Frequência em data ------------------------------------------------------

ftd <- function(n, freq) {
  if(freq != "D") {
    freqs <- c("Q" = 24,
               "M" = 12,
               "T" = 4,
               "S" = 2,
               "A" = 1)
    freq <- freqs[freq]
    ano <- floor(n)
    mes <- round((n-ano)*12+1,0)
    dia = "1"
    if (freq == 24) dia = ifelse(mes*2 %% 2 == 0, "1", "15")
    data <- as.Date(paste0(ano, "-", mes, "-", dia))
    
    return(data)
  } 
  
  data <- as.Date(n)
  
  return(data)
}

dtf <- function(date, freq) {
  freqs <- c("Q" = 24,
             "M" = 12,
             "T" = 4,
             "S" = 2,
             "A" = 1)
  mes <- lubridate::month(date)
  dia <- lubridate::day(date)
  dia <- (dia-1)/14 # 0 ou 1
  return((mes-1)*freqs[freq]/12 + dia + 1)
}


# Extrai datas para o focus -----------------------------------------------

focus_datas <- function(datas, dia) {
  datas <- as.Date(datas)
  first_date <- min(datas, na.rm = TRUE)
  current_date <- max(datas, na.rm = TRUE)
  
  if(!(dia %in% c("Relatório anterior", "Primeiro do ano"))) {
    return(current_date)
  }
  
  list_weeks <- data.frame(date = seq.Date(from = first_date, 
                                           to = current_date, 
                                           by = "1 day"), 
                       monday = weekdays(date) == "segunda-feira", 
                       week_number = 1 + cumsum(monday))
  
  datas_weeks <- dplyr::left_join(data.frame(date = datas), list_weeks, by = "date") 
  current_week <- as.numeric(datas_weeks[datas_weeks$date == current_date, 3])
  
  if(dia == "Relatório anterior") {
    semana <- current_week-1
    datas_weeks <- datas_weeks[datas_weeks$week_number == semana,]
    return(max(datas_weeks$date, na.rm = TRUE))
  }
  
  if(dia == "Primeiro do ano") {
    datas_weeks <- datas_weeks[lubridate::year(datas_weeks$date) == lubridate::year(Sys.date()),]
    datas_weeks <- datas_weeks[datas_weeks$week_number == min(datas_weeks$week_number, na.rm = TRUE),]
    return(max(datas_weeks$date, na.rm = TRUE))
  }
}
