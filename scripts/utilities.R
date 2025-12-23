# Aglutina ----------------------------------------------------------------

aglut <- function(x) {
  # aglutina valores de um vetor
  x <- as.numeric(x)  
  non_na <- x[!is.na(x)]
  if (length(non_na) > 0) {
    return(non_na[1])
  } else {
    return(NA)
  }
}

# Print -------------------------------------------

print.macro_serie <- function(x, comp = NULL, ...) {
  # print para objeto macro_serie
  
  if(is.null(comp)) comp = 12
  
  dados <- round(as.numeric(x), 2)
  freq <- rec_frequencia(x)
  
  if(inherits(x, "mts")) {
    class(dados) <- c("mts", "ts")
    attr(dados, "dim") <- attr(x, "dim")
    attr(dados, "dimnames") <- attr(x, "dimnames")
    attr(dados, "tsp") <- attr(x, "tsp")
    print(tail(dados, comp))
    invisible(dados)
  }
  
  if(inherits(x, "ts")) {
    class(dados) <- "ts"
    attr(dados, "tsp") <- attr(x, "tsp")
    fnums <- c("Q" = 24, "M" = 12, "T" = 4, "S" = 2, "A" = 1)
    print(tail(dados, comp*fnums[freq]))
    invisible(dados)
  }
  
  if(inherits(x, "zoo")) {
    class(dados) <- "zoo"
    attr(dados, "index") <- attr(x, "index")
    print(tail(dados, comp))
    invisible(dados)
  }
}

# Janela ------------------------------------------------------------------

janela <- function(x, inicio = NULL, fim = NULL, nonNA = TRUE) {
  # window para objeto macro_serie
  
  freq <- rec_frequencia(x)
  attrib <- attributes(x)
  inicio_serie <- rec_inicio(x)
  fim_serie <- rec_fim(x)
  
  freqs <- c(
    "D" = 365,
    "Q" = 24,
    "M" = 12,
    "T" = 4,
    "S" = 2,
    "A" = 1
  )
  
  if (freq %in% names(freqs)) {
    if(!is.null(inicio)) {
      inicio = as.Date(inicio)
      
      if(inicio > fim_serie) {
        return(NULL)
      }
      
      if(inicio >= inicio_serie) {
        inicio_serie <- inicio
      } 
      
      inicio_ano <- lubridate::year(inicio)
      if (freq == "Q") {
        if(!(lubridate::day(inicio) %in% c(1, 15))) {
          stop("Data de início inválida para frequência quinzenal")
        } 
        
        inicio_freq <- (lubridate::month(inicio) - 1)*2 + (2 - lubridate::day(inicio) %% 15)
      } else {
        inicio_freq <- (lubridate::month(inicio)-1)*12/freqs[freq] + 1
      }
      
      inicio_vec <- c(inicio_ano, inicio_freq)
    } else {
      if(nonNA) {
        inicio_vec <-  time(x)[which(!is.na(x))[1]]
        if(freq != "Q") {
          inicio_serie <- as.Date(paste(floor(inicio_vec), 
                                        round(((inicio_vec-floor(inicio_vec))*12)+1), 
                                        1, sep = "-"))
        } else {
          inicio_dia <- ifelse(round((inicio_vec-floor(inicio_vec))*24) %% 2 == 0, 1, 15)
          inicio_serie <- as.Date(paste(floor(inicio_vec), 
                                        round(((inicio_vec-floor(inicio_vec))*12)+1), 
                                        inicio_dia, sep = "-"))
        }
      } else {
        inicio_vec = NULL
      }
    }
    
    
    if(!is.null(fim)) {
      fim = as.Date(fim)
      
      if(fim < inicio_serie) {
        return(NULL)
      }
      
      if(fim <= fim_serie) {
        fim_serie <- fim
      }
      
      fim_ano <- lubridate::year(fim)
      if (freq == "Q") {
        if(!(lubridate::day(fim) %in% c(1, 15))) {
          stop("Data de fim inválida para frequência quinzenal")
        } 
        
        fim_freq <- (lubridate::month(fim) - 1)*2 + (2 - lubridate::day(fim) %% 15)
      } else {
        fim_freq <- (lubridate::month(fim)-1)*12/freqs[freq] + 1
      }
      fim_vec <- c(fim_ano, fim_freq)
    } else {
      fim_vec = NULL
    }
  } 
  
  serie <- x
  class(serie) <- setdiff(class(serie), "macro_serie")
  
  suppressWarnings({
    serie <- window(serie, 
                    start = inicio_vec,
                    end = fim_vec)
  })
  
  attr_serie <- attributes(serie)[setdiff(names(attributes(serie)), "class")]
  attrib[names(attr_serie)] <- attr_serie
  attributes(serie) <- attrib
  attr(serie, "inicio") <- as.character(inicio_serie)
  attr(serie, "fim") <- as.character(fim_serie)
  
  return(serie)
}

