# Média móvel de n períodos ----------------------------------------------------------------

media_movel_n <- function(ms, n) {
  attrs <- attributes(ms)
  unid <- attrs$unidade
  x <- as.numeric(ms)
  
  if(unid == "%") {
    f = function(x) {
      val = (exp(mean(log(1+x/100)))-1)*100
      return(val)
    }
  } else {
    f = mean
  }
  
  x <- zoo::rollapply(x, width = n, FUN = f, 
                      fill = NA, align = "right")
  attributes(x) <- attrs
  
  return(x)
}

# Acumulado de n períodos ----------------------------------------------------------------

acumulado_n <- function(ms, n) {
  attrs <- attributes(ms)
  unid <- attrs$unidade
  x <- as.numeric(ms)
  
  if(unid == "%") {
    f = function(x) {
      val = (exp(sum(log(1+x/100)))-1)*100
      return(val)
    }
  } else {
    f = sum
  }
  
  x <- zoo::rollapply(x, width = n, FUN = f, 
                      fill = NA, align = "right")
  attributes(x) <- attrs
  
  return(x)
}

# Acumulado de n períodos ----------------------------------------------------------------

acumulado_ano <- function(ms) {
  attrs <- attributes(ms)
  freq <- rec_frequencia(ms)
  
  if(freq == "D") {
    message("Acumulado no ano não é aplicável a serie diária.")
    return(ms)
  }
  
  unid <- rec_unidade(ms)
  x <- as.numeric(ms)
  ts <- time(ms)
  ano = floor(ts)
  df <- data.frame(ano = ano, x = x)
  
  if(unid == "%") {
    f = function(x) {
      val = (exp(cumsum(log(1+x/100)))-1)*100
      return(val)
    }
  } else {
    f = cumsum
  }
  
  df <- dplyr::group_by(df, ano)
  df <- dplyr::mutate(df, x = f(x))
  x <- df$x
  
  attributes(x) <- attrs
  
  return(x)
}

# Acumulado total ---------------------------------------------------------

acumulado <- function(ms) {
  attrs <- attributes(ms)
  unid <- attrs$unidade
  x <- as.numeric(ms)
  
  if(unid == "%") {
    f = function(x) {
      val = (exp(cumsum(log(1+x/100)))-1)*100
      return(val)
    }
  } else {
    f = cumsum
  }
  
  x <- f(x)
  attributes(x) <- attrs
  
  return(x)
}

# Anualizado --------------------------------------------------------------

anualizado <- function(ms) {
  attrs <- attributes(ms)
  unid <- attrs$unidade
  freq <- attrs$frequencia
  
  if(freq == "D") {
    message("Série diária não pode ser acumulada.")
    return(ms)
  }
  
  freqs <- c(
    "Q" = 12,
    "M" = 12,
    "T" = 4,
    "S" = 2,
    "A" = 1
  )
  
  x <- as.numeric(ms)
  
  if(unid == "%") {
    x = ((1+x/100)^freqs[freq]-1)*100
  } else {
    x = freqs[freq]*x
  }
  
  attributes(x) <- attrs
  
  return(x)
}


# Shift -------------------------------------------------------------------

desloca <- function(ms, n) {
  # Desloca a série n periodo para frente/trás
  freq <- rec_frequencia(ms)
  inicio <- rec_inicio(ms)
  fim <- rec_fim(ms)
  attrs <- attributes(ms)
  
  if(freq == "D") {
    message("Desloca não é aplicável para séries diárias.")
    return(ms)
  }
  
  x <- stats::lag(ms, n)
  x <- janela(x, inicio = inicio,
              fim = fim)
  
  return(x)
}


# Período -----------------------------------------------------------------

datas <- function(ms, formato = "%b/%y") {
  # retorna o período em forma numérica e o atributo unidade com o formato 
  # especificado
  attrs <- attributes(ms)
  x <- as.numeric(time(ms))
  
  attributes(x) <- attrs
  attr(x, "medida") <- "Tempo"
  attr(x, "unidade") <- formato
  
  return(x)
}

# Diferença ---------------------------------------------------------------

diferenca <- function(ms, n) {
  #retorna a série na diferença
  attrs <- attributes(ms)
  inicio <- rec_inicio(ms)
  freq <- rec_frequencia(ms)
  inicio <- Reduce(function(acc, ...) prox_periodo(acc, freq), x = seq_len(n), init = inicio)
  
  x <- abs(diff(ms, lag = n))
  
  attributes(x) <- c(attributes(x),attrs[setdiff(names(attrs), names(attributes(x)))])
  attr(x, "inicio") <- inicio
  
  return(x)
}

# Modifica séries ---------------------------------------------------------

modifica_series <- function(ms, mods) {
  if(is.na(mods)) {
    return(ms)
  }
  
  lista_funs <- list(
    "12 meses" = function(x)
      anualizado(media_movel_n(x, n = 12)),
    "Acumulado" = acumulado,
    "MM3m" = function(x)
      media_movel_n(x, n = 3),
    "MM3ma" = function(x)
      anualizado(media_movel_n(x, n = 3)),
    "Anualizado"  = anualizado,
    "m1" = function(x) 
      desloca(x, n = -1),
    "m4" = function(x) 
      desloca(x, n = -4),
    "m12" = function(x) 
      desloca(x, n = -12),
    "dia" = function(x)
      datas(x, formato = "%d"),
    "mes" = function(x)
      datas(x, formato = "%B"),
    "ano" = function(x)
      datas(x, formato = "%Y"),
    "mesano" = function(x)
      datas(x, formato = "%B de %Y"),
    "diamesano" = function(x)
      datas(x, formato = "%d de %B de %Y"),
    "bb" = function(x)
      datas(x, formato = "%b"),
    "bbyy" = function(x)
      datas(x, formato = "%b/%y"),
    "bbyyyy" = function(x)
      datas(x, formato = "%b/%Y"),
    "mm" = function(x)
      datas(x, formato = "%m"),
    "mmyy" = function(x)
      datas(x, formato = "%m/%y"),
    "mmyyyy" = function(x)
      datas(x, formato = "%m/%Y"),
    "ddmmyyyy" = function(x)
      datas(x, formato = "%d/%m/%Y"),
    "d1" = function(x)
      diferenca(x, n = 1),
    "d12" = function(x)
      diferenca(x, n = 12),
    "Acumulado ano" = function(x)
      acumulado_ano(x)
  )
  
  mods <- unlist(lapply(strsplit(mods, "&"), trimws))
  
  for (nm in mods) {
    f <- lista_funs[[nm]]
    ms <- f(ms)
  }
  
  return(ms)
}

