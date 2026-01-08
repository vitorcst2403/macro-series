# Print -------------------------------------------

print.macro_serie <- function(ms, ...) {
  # print para objeto macro_serie
  serie <- ms$serie
  serie <- round(serie, 2)
  print(serie)
}

# Janela ------------------------------------------------------------------

janela <- function(...) {
  UseMethod("janela")
}

janela.default <- function(...) {
  stop("Objeto deve ser de classe 'macro_serie'.")
}

janela.macro_serie <- function(ms, inicio = NULL, fim = NULL, nonNA = TRUE) {
  # window para objeto macro_serie
  
  meta <- ms$meta
  
  freq <- meta$frequencia
  inicio_serie <- meta$inicio
  if(is.null(inicio)) {
    inicio <- inicio_serie
  } 
  inicio <- as.Date(inicio)
  fim_serie <- meta$fim
  if (is.null(fim)) {
    fim <- fim_serie
  }
  fim <- as.Date(fim)
  
  datas <- gera_datas(freq = freq,
                      inicio = inicio,
                      fim = fim)
  
  serie <- ms$serie
  
  if(inherits(serie, "ts")) {
    serie <- window(serie, 
                    start = c(lubridate::year(inicio), dtf(inicio, freq)),
                    end = c(lubridate::year(fim), dtf(fim, freq))
    )
    if(nonNA) {
      inicio_vec <- time(serie)[!is.na(serie)][1]
      fim_vec <- tail(time(serie)[!is.na(serie)], 1)
      
      inicio <- ftd(inicio_vec, freq)
      fim <- ftd(fim_vec, freq)
      
      serie <- window(serie, 
                      start = inicio_vec,
                      end = fim_vec)
    }
  }
  
  if(inherits(serie, "zoo")) {
    serie <- window(serie,
                    start = inicio,
                    end = fim)
    
    if(nonNA) {
      inicio_vec <- index(serie)[!is.na(serie)][1]
      fim_vec <- tail(index(serie)[!is.na(serie)], 1)
      
      serie <- window(serie, 
                      start = inicio_vec,
                      end = fim_vec)
    }
  }
  
  meta$inicio <- inicio
  meta$fim <- fim
  
  ms <- list(serie = serie,
             meta = meta)
  class(ms) <- "macro_serie"
  
  return(ms)
}

# Funções customizadas ----------------------------------------------------

# Aglutina
aglut <- function(x) {
  x <- as.numeric(x)  
  non_na <- x[!is.na(x)]
  if (length(non_na) > 0) {
    return(non_na[1])
  } else {
    return(NA)
  }
}

# Contribuição IPCA
contr_fun <- function(x) {
  x[1]*x[2]/100
}

# Inflação relativa
inflarel_fun <- function(x) {
  (1+x[1]/100)/(1+x[2]/100)-1
}

# Difusão
dif_fun <- function(x) {
  sum(x>0)/length(x)*100
}

# Participação percentual
part_fun <- function(x) {
  x[1]/x[2]*100
}

