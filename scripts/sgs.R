# SGS ---------------------------------------------------------------------

sgs <- function(codigo, 
                freq,
                inicio_diario = NULL) {
  # Função para extrair séries do SGS do BACEN  
  retry_fun <- function(x) httr::RETRY(
    "GET",
    x,
    times = 3,
    pause_base = 2,
    pause_cap = 30,
    pause_min = 1
  )
  
  series_name <- paste0(codigo, "_sgs")
  
  ## Tenta puxar a série do cache 
  if (exists(series_name, env = .cache_env)) {
    
    dados <- get(series_name, envir = .cache_env)
    
    return(dados)
  }
  
  ## Se não encontra a série no cache, puxa do enderço do API 
  
  ### Série não diária 
  if (freq != "D") {
    url <- paste0(
      "https://api.bcb.gov.br/dados/serie/bcdata.sgs.",
      codigo,
      "/dados?formato=json"
    )
    
    response <- retry_fun(url)
    error <- httr::http_error(response)
    
    if(error) {
      return(NULL)
    }
    
    dados <- httr::content(response, as = "parsed", type = "application/json")
    dados <- lapply(dados, function(x) {
      lst <- list(data = as.Date(x$data, format = "%d/%m/%Y"),
                  valor = as.numeric(x$valor))
      return(lst)
    })
    dados <- do.call(rbind, lapply(dados, as.data.frame))
    dados <- dplyr::arrange(dados, data)
    row.names(dados) <- NULL
    
    # Salva a série cheia no cache
    assign(series_name, dados, envir = .cache_env)
    
    return(dados)
  }
  
  ### Série diária 
  if(!is.null(inicio_diario)) {
    inicio_diario <- as.Date(inicio_diario)
  }
  
  dados <- list()
  erros <- list()
  data_final <- Sys.Date()
  
  data_inicial <- data_final - lubridate::years(10) + 1
  error = FALSE
  
  while (!error) {
    url <- paste0(
      "https://api.bcb.gov.br/dados/serie/bcdata.sgs.",
      codigo,
      "/dados?formato=json&dataInicial=",
      format(data_inicial, "%d/%m%/%Y"),
      "&dataFinal=",
      format(data_final, "%d/%m%/%Y")
    )
    
    response <- retry_fun(url)
    error <- httr::http_error(response)
    
    if (error) {
      break
    } 
    
    erros <- c(erros, error)
    res <- httr::content(response, as = "parsed", type = "application/json")
    dados <- c(dados, res)
    
    data_final <- data_final - lubridate::years(10)
    if(data_final < inicio_diario) {
      break
    }
    data_inicial <- data_final - lubridate::years(10) + 1
  }
  
  if(length(erros) == 0) {
    return(NULL)
  }
  
  dados <- Filter(function(x) !is.null(x$valor), dados)
  dados <- lapply(dados, function(x) {
    lst <- list(data = as.Date(x$data, format = "%d/%m/%Y"),
                valor = as.numeric(x$valor))
    return(lst)
  })
  dados <- do.call(rbind, lapply(dados, as.data.frame))
  dados <- dplyr::arrange(dados, data)
  row.names(dados) <- NULL
  
  # Salva a série cheia no cache
  assign(series_name, dados, envir = .cache_env)
  
  return(dados)
}


