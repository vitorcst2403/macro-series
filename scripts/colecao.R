coleta_series <- function(series, medidas, frequencias, territorios,
                          inicio = NULL) {
  # validações e preparação de vetores
  
  if (!is.character(series)) {
    stop("'series' precisa ser vetor de caracteres.")
  }
  
  n <- length(series)
  
  if (!is.null(medidas)) {
    if (length(medidas) == 1) {
      medidas <- rep(medidas, n)
    } else if (length(medidas) != n) {
      stop("'medidas' deve ter tamanho 1 ou o mesmo de 'series'.")
    }
  } else {
    stop("Medidas devem ser definidas")
  }
  
  if (!is.null(frequencias)) {
    if (length(frequencias) == 1) {
      frequencias <- rep(frequencias, n)
    } else if (length(frequencias) != n) {
      stop("'frequencias' deve ter tamanho 1 ou o mesmo de 'series'.")
    }
  } else {
    stop("Frequencias devem ser definidas")
  }
  
  if (!is.null(territorios)) {
    if (length(territorios) == 1) {
      territorios <- rep(territorios, n)
    } else if (length(territorios) != n) {
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
  
  serie_repos <- list()
  
  for (i in 1:n) {
    if (is.null(territorios[i])) {
      serie_repos[[i]] <- paste(tolower(series[i]), sufixos_medi(medidas[i]), tolower(frequencias[i]), sep = "_")
    } else {
      serie_repos[[i]] <- paste(tolower(series[i]), sufixos_medi(medidas[i]), tolower(frequencias[i]), sufixos_terri(territorios[i]), sep = "_") 
    }
  }
  
  ms_list <- lapply(serie_repos, function(repos) {
    extrai_safe(repos, 
                inicio = inicio,
                atual = TRUE)
  })
  
  if (sum(sapply(ms_list, is.null)) > 0) {
    warning("Erro na coleção das séries. ")
    return(NULL)
  }
  
  ms <- Reduce(function(x, y)
    combina_serie(x, y), ms_list)
  
  return(ms)
}
