ms_dados <- function(dados, 
                     regra) {
  # Converte um dataframe com coluna 'data' e 'valor' em um objeto 'macro_serie' com base em uma regra de formação 
  freq <- rec_frequencia(regra)
  
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
    
    serie <- ts(
      data = dados$valor,
      start = c(lubridate::year(inicio), inicio_freq),
      frequency = freqs[freq]
    )
    
    attr_serie <- attributes(serie)[setdiff(names(attributes(serie)), "class")]
    attr_regra <- attributes(regra)[setdiff(names(attributes(regra)), c("class", "names"))]
    class_serie <- c("macro_serie", class(serie))
    
    attributes(serie) <- c(list("class" = class_serie), attr_serie, attr_regra)
    attr(serie, "fim") <- as.character(fim)
    
    return(serie)
  }
  
  fim <- max(dados$data)
  
  serie <- zoo::zoo(x = dados$valor, 
                    order.by = dados$data)
  
  attr_serie <- attributes(serie)[setdiff(names(attributes(serie)), "class")]
  attr_regra <- attributes(regra)[setdiff(names(attributes(regra)), c("class", "names"))]
  class_serie <- c("macro_serie", class(serie))
  
  attributes(serie) <- c(list("class" = class_serie), attr_serie, attr_regra)
  attr(serie, "fim") <- as.character(fim)
  
  return(serie)
}


# Combina séries ------------------------------

combina_serie <- function(x1, x2, ...) {
  UseMethod("combina_serie")
}

combina_serie.default <- function(x1, x2) {
  stop("Método válido somente para objeto macro_serie")
}

combina_serie.macro_serie <- function(x1, x2, fill = TRUE) {
  if(is.null(x2)) {
    return(x1)
  }
  
  if (!inherits(x2, "macro_serie")) {
    stop("Os dois argumentos devem ser de classe 'macro_serie'")
  }
  
  freq1 <- rec_frequencia(x1)
  freq2 <- rec_frequencia(x2)
  
  freqs <- c("D" = 365, 
             "Q" = 24, 
             "M" = 12, 
             "T" = 4, 
             "S" = 2, 
             "A" = 1)
  
  freq <- names(freqs[freqs == max(freqs[freq1], freqs[freq2])])
  
  ini1 <- rec_inicio(x1)
  ini2 <- rec_inicio(x2)
  ini <- min(ini1, ini2)
  
  fim1 <- rec_fim(x1)
  fim2 <- rec_fim(x2)
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
  
  df1 <- data.frame(cbind(data = lista_datas(x1), as.data.frame(x1)))
  df2 <- data.frame(cbind(data = lista_datas(x2), as.data.frame(x2)))
  df <- data.frame(data = data)
  df <- dplyr::left_join(df, df1, by = "data")
  df <- dplyr::left_join(df, df2, by = "data")
  df <- df[setdiff(names(df), "data")]
  colnames(df) <- c(rec_serie(x1), rec_serie(x2))
  
  if(fill) {
    if(freq != "D") {
      
      for(cols in names(df)) {
        df[[cols]] <- zoo::na.locf(df[[cols]], na.rm = FALSE)
      }
    } else {
      for(cols in names(df)) {
        df[[cols]] <- zoo::na.approx(df[[cols]], na.rm = FALSE)
      }
    }
  }
  
  if(freq != "D") {
    inicio_ano <- lubridate::year(ini)
    if(freq == "Q") {
      inicio_freq <- (lubridate::month(ini) - 1)*2 + (2 - lubridate::day(ini) %% 15)
    } else {
      inicio_freq <- (lubridate::month(ini)-1)*12/freqs[freq] + 1
    }
    
    mserie <- ts(data = df,
                 start = c(inicio_ano, inicio_freq),
                 frequency = freqs[freq])
  } else {
    mserie <- zoo::zoo(df, order.by = data)
  }
  
  dimnames(mserie) <- list(NULL, c(attr(x1, "descricao"), attr(x2, "descricao")))
  
  attrs <- c("tema", "descricao", "serie", "unidade",  
             "medida", "territorio")
  
  attributes(mserie) <- c(attributes(mserie), setNames(lapply(attrs, function(a) {
    c(as.character(attributes(x1)[[a]]), 
      as.character(attributes(x2)[[a]]))
  }), attrs))
  
  attr(mserie, "inicio") <- as.character(ini)
  attr(mserie, "fim") <- as.character(fim)
  attr(mserie, "frequencia") <- freq
  class(mserie) <- c("macro_serie", class(mserie))
  
  return(mserie)
}

# Aplica função em séries ------------------------------------
aplica_serie <- function(serie, fun, ...) {
  UseMethod("aplica_serie")
}

aplica_serie.default <- function(serie, fun) {
  stop("Método válido somente para objeto macro_serie")
}

aplica_serie.macro_serie <- function(serie, 
                                     fun, 
                                     roll = TRUE,
                                     width = 1, 
                                     align = "right", 
                                     fill = NA, 
                                     partial = FALSE,
                                     ...) {
  attrs <- attributes(serie)
  
  if(is.null(dim(serie))) {
    serie <- matrix(serie, ncol = 1)
  } 
  
  if (roll) {
    ms <- apply(serie, 2, function(col) {
      zoo::rollapply(col, 
                     width = width,
                     FUN = fun,
                     align = align,
                     fill = fill,
                     partial = partial)
    })
  } else {
    ms <- apply(serie, 2, function(col) fun(col))
  }
  
  attributes(ms) <- attrs
  
  return(ms)
}

# Resume séries por função ------------------------------------
resume_serie <- function(x, fun, ...) {
  UseMethod("resume_serie")
}

resume_serie.default <- function(x, fun, ...) {
  stop("Método válido somente para objeto macro_serie")
}

resume_serie.macro_serie <- function(x, 
                                     fun, 
                                     ...) {
  dots <- match.call(expand.dots = FALSE)$...
  
  if (length(dots) == 0) {
    extra_args <- NULL
  } else {
    extra_args <- list(...)
  }
  
  temas <- rec_tema(x)
  if(length(unique(temas)) == 1) {
    tema <- unique(temas)
  } else {
    tema = NULL
  }
  
  unis <- rec_unidade(x)
  if(length(unique(unis)) == 1) {
    unidade <- unique(unis)
  } else {
    unidade = NULL
  }
  
  medis <- rec_medida(x)
  if(length(unique(medis)) == 1) {
    medida <- unique(medis)
  } else {
    medida = NULL
  }
  
  terris <- rec_territorio(x)
  if(length(unique(terris)) == 1) {
    territorio <- unique(terris)
  } else {
    territorio = NULL
  }
  
  freq <- rec_frequencia(x)
  ini <- rec_inicio(x)
  fim <- rec_fim(x)
  
  if(is.null(dim(x))) {
    x <- matrix(x, ncol = 1)
  } 
  
  ms <- apply(x, 1, function(row) {
    tryCatch({
      env <- new.env()
      env$x <- row
      
      if(!is.null(extra_args)) {
        list2env(extra_args, envir = env)
      }
      
      eval(parse(text = fun), envir = env)
    }, error = function(e) NA)
  })
  
  if(is.null(dim(ms))) {
    matrix = FALSE
  } else {
    matrix = TRUE
  }
  
  if(matrix) {
    ms <- t(ms)
  }
  
  if(all(is.na(ms))) {
    return(NULL)
  }
  
  if(freq != "D") {
    if(matrix) {
      dim_ms <- dim(ms)[2]
      
      attr(ms, "tema") <- rep(tema, dim_ms)
      attr(ms, "descricao") <- paste0("Série calculada ", 1:dim_ms)
      attr(ms, "serie") <- paste0("calc_", 1:dim_ms)
      attr(ms, "unidade") <- rep(unidade, dim_ms)
      attr(ms, "medida") <- rep(medida, dim_ms)
      attr(ms, "inicio") <- ini
      attr(ms, "fim") <- fim
      attr(ms, "frequencia") <- freq
      attr(ms, "class") <- c("macro_serie", "mts", "ts", "matrix", "array")
      attr(ms, "tsp") <- attributes(x)$tsp
    } else {
      attr(ms, "tema") <- tema
      attr(ms, "descricao") <- "Série calculada"
      attr(ms, "serie") <- "calc_1"
      attr(ms, "unidade") <- unidade
      attr(ms, "medida") <- medida
      attr(ms, "inicio") <- ini
      attr(ms, "fim") <- fim
      attr(ms, "frequencia") <- freq
      attr(ms, "class") <- c("macro_serie", "ts")
      attr(ms, "tsp") <- attributes(x)$tsp
    }
  } else {
    attr(ms, "tema") <- rep(tema, dim_ms)
    attr(ms, "descricao") <- paste0("Série calculada ", 1:dim_ms)
    attr(ms, "serie") <- paste0("calc_", 1:dim_ms)
    attr(ms, "unidade") <- rep(unidade, dim_ms)
    attr(ms, "medida") <- rep(medida, dim_ms)
    attr(ms, "inicio") <- ini
    attr(ms, "fim") <- fim
    attr(ms, "frequencia") <- freq
    attr(ms, "class") <- c("macro_serie", "zoo")
  }
  
  return(ms)
}

