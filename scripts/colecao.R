coleta_series <- function(series, medidas = NULL, frequencias = NULL, territorios = NULL,
                          inicio = NULL, ignora_nulo = TRUE, .local_cache) {
  # validações e preparação de vetores
  
  if (!is.character(series)) {
    stop("'series' precisa ser vetor de caracteres.")
  }
  
  n <- length(series)
  
  if (!is.null(medidas)) {
    if (length(medidas) == 1) {
      medidas <- rep(medidas, length(series))
    } else if (length(medidas) != length(series)) {
      stop("'medidas' deve ter tamanho 1 ou o mesmo de 'series'.")
    }
  } else {
    medidas <- rep(list(NULL), n)
  }
  
  if (!is.null(frequencias)) {
    if (length(frequencias) == 1) {
      frequencias <- rep(frequencias, length(series))
    } else if (length(frequencias) != length(series)) {
      stop("'frequencias' deve ter tamanho 1 ou o mesmo de 'series'.")
    }
  } else {
    frequencias <- rep(list(NULL), n)
  }
  
  if (!is.null(territorios)) {
    if (length(territorios) == 1) {
      territorios <- rep(territorios, length(series))
    } else if (length(territorios) != length(series)) {
      stop("'territorios' deve ter tamanho 1 ou o mesmo de 'series'.")
    }
  } else {
    territorios <- rep(list(NULL), n)
  }
  
  if (!is.null(inicio)) {
    inicios <- rep(inicio, n)
  } else {
    inicios <- rep(list(NULL), n)
  }
  
  # função que define a regra e realiza a extração
  
  args_list <- list(
    serie = series,
    medida = medidas,
    frequencia = frequencias,
    territorio = territorios,
    inicio = inicios
  )
  
  ms_list <- purrr::pmap(args_list, function(serie, medida, frequencia, territorio, inicio) {
    extrai_safe(serie, medida, frequencia, territorio, inicio, 
                ignora_nulo = ignora_nulo, .local_cache = .local_cache)
  })
  
  names(ms_list) <- series
  
  if (!ignora_nulo && sum(sapply(ms_list, is.null)) > 0) {
    return(NULL)
  }
  
  if (sum(sapply(ms_list, is.null)) == length(ms_list)) {
    return(NULL)
  }
  
  ms_list <- Filter(function(x)
    !is.null(x), ms_list)
  
  ms <- Reduce(function(x, y)
    combina_serie(x, y, fill = FALSE), ms_list)
  
  return(ms)
}
