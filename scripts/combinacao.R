ms_dados <- function(dados, 
                     regra) {
  # Converte um dataframe com coluna 'data' e 'valor' em um objeto 'macro_serie' com base em uma regra de formação 
  
  meta <- regra$meta
  
  freq <- meta$frequencia
  
  freqs <- c(
    "D" = 365,
    "Q" = 24,
    "M" = 12,
    "T" = 4,
    "S" = 2,
    "A" = 1
  )
  
  if (freq != "D") {
    inicio <- min(dados$data)
    fim <- max(dados$data)
    
    data <- gera_datas(freq = freq,
                       inicio = inicio,
                       fim = fim)
    
    if(freq == "Q") {
      inicio_freq <- (lubridate::month(inicio) - 1)*2 + (2 - lubridate::day(data[1]) %% 15)
    } else {
      inicio_freq <- (lubridate::month(inicio)-1)*12/freqs[freq] + 1
    }
    
    dados <- dplyr::left_join(data.frame(data = data), dados, by = "data")
    
    fim <- max(dados$data)
    
    serie <- ts(
      data = dados$valor,
      start = c(lubridate::year(inicio), inicio_freq),
      frequency = freqs[freq]
    )
    
    meta <- c(meta,
              list(fim = fim))
    
    ms <- list(serie = serie,
               meta = meta)
    
    class(ms) <- "macro_serie"
    
    return(ms)
  }
  
  fim <- max(dados$data)
  
  serie <- zoo::zoo(x = dados$valor, 
                    order.by = dados$data)
  
  meta <- c(meta,
            list(fim = fim))
  
  ms <- list(serie = serie,
             meta = meta)
  
  class(ms) <- "macro_serie"
  
  return(ms)
}


# Combina séries ------------------------------

combina_serie <- function(x1, x2, ...) {
  UseMethod("combina_serie")
}

combina_serie.default <- function(x1, x2) {
  stop("Método válido somente para objeto macro_serie")
}

combina_serie.macro_serie <- function(x1, x2) {
  if(is.null(x2)) {
    return(x1)
  }
  
  if (!inherits(x2, "macro_serie")) {
    stop("Os dois argumentos devem ser de classe 'macro_serie'")
  }
  
  freq1 <- x1$meta$frequencia
  freq2 <- x2$meta$frequencia
  
  freqs <- c("D" = 365, 
             "Q" = 24, 
             "M" = 12, 
             "T" = 4, 
             "S" = 2, 
             "A" = 1)
  
  freq <- names(freqs[freqs == max(freqs[freq1], freqs[freq2])])
  
  ini1 <- x1$meta$inicio
  ini2 <- x2$meta$inicio
  ini <- min(ini1, ini2)
  
  fim1 <- x1$meta$fim
  fim2 <- x2$meta$fim
  fim <- max(fim1, fim2)
  
  data <- NULL
  
  if(freq != "D") {
    data <- gera_datas(freq = freq,
                       inicio = ini,
                       fim = fim)
  }
  
  if(is.null(data)) {
    data <- as.Date(sort(unique(lista_datas(x1),
                                lista_datas(x2))))
  }
  
  df1 <- data.frame(cbind(data = lista_datas(x1), as.data.frame(x1$serie)))
  df2 <- data.frame(cbind(data = lista_datas(x2), as.data.frame(x2$serie)))
  df <- data.frame(data = data)
  df <- dplyr::left_join(df, df1, by = "data")
  df <- dplyr::left_join(df, df2, by = "data")
  df <- df[setdiff(names(df), "data")]
  
  if(freq != "D") {
    inicio_ano <- lubridate::year(ini)
    if(freq == "Q") {
      inicio_freq <- (lubridate::month(ini) - 1)*2 + (2 - lubridate::day(ini) %% 15)
    } else {
      inicio_freq <- (lubridate::month(ini)-1)*12/freqs[freq] + 1
    }
    
    serie <- ts(data = df,
                 start = c(inicio_ano, inicio_freq),
                 frequency = freqs[freq])
  } else {
    serie <- zoo::zoo(df, order.by = data)
  }
  
  meta <- Map(c, x1$meta, x2$meta)
  ms <- list(serie = serie,
             meta = meta)
  
  class(ms) <- "macro_serie"
  
  return(ms)
}

# Aplica função em séries ------------------------------------
aplica_serie <- function(ms, fun, ...) {
  UseMethod("aplica_serie")
}

aplica_serie.default <- function(ms, fun) {
  stop("Método válido somente para objeto macro_serie")
}

aplica_serie.macro_serie <- function(ms, 
                                     fun, 
                                     roll = TRUE,
                                     width = 1, 
                                     align = "right", 
                                     fill = NA, 
                                     partial = FALSE,
                                     ...) {
  
  serie <- ms$serie
  meta <- ms$meta
  
  if(is.null(dim(serie))) {
    serie <- matrix(serie, ncol = 1)
  } 
  
  if (roll) {
    serie <- apply(serie, 2, function(col) {
      zoo::rollapply(col, 
                     width = width,
                     FUN = fun,
                     align = align,
                     fill = fill,
                     partial = partial)
    })
  } else {
    serie <- apply(serie, 2, function(col) fun(col))
  }
  
  ms <- lista(serie = serie,
              meta = meta)
  
  return(ms)
}

# Resume séries por função ------------------------------------
resume_serie <- function(ms, fun, ...) {
  UseMethod("resume_serie")
}

resume_serie.default <- function(ms, fun, ...) {
  stop("Método válido somente para objeto macro_serie")
}

resume_serie.macro_serie <- function(ms, 
                                     fun, 
                                     ...) {
  dots <- list(...)
  extra_args <- if (length(dots) == 0) list() else dots
  
  serie <- ms$serie
  
  if(is.null(dim(serie))) {
    serie <- matrix(serie, ncol = 1)
  } 
  
  # prepara função que será aplicada a cada linha:
  if (is.function(fun)) {
    fun_fn <- function(row) {
      tryCatch({
        do.call(fun, c(list(row), extra_args))
      }, error = function(e) NA)
    }
  } else {
    stop("resume_serie: 'fun' deve ser função.")
  }
  
  # aplica por linha
  serie_res <- apply(serie, 1, function(row) fun_fn(row))
  
  # determina formato do resultado
  is_matrix <- !is.null(dim(serie_res))
  if (is_matrix) {
    serie_res <- t(serie_res)
  } 
  
  if(all(is.na(serie_res))) {
    return(NULL)
  }
  
  # atribui atributos de saída coerentes com tipo (ts / zoo)
  if(inherits(ms$serie, "ts")) {
    serie <- ts(
      data = serie_res,
      start = start(ms$serie),
      frequency = frequency(ms$serie)
    )
  } else {
    serie <- zoo::zoo(x = serie_res, 
             order.by = index(ms$serie))
  }
  
  meta <- list(
    descricao = "Série combinada"
  )
  
  ms <- list(serie = serie,
             meta = meta)
  
  class(ms) <- "macro_serie"
  
  return(ms)
}
