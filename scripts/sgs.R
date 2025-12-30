# SGS ---------------------------------------------------------------------
sgs <- function(codigo,
                freq,
                inicio_diario = NULL) {
  # Função para extrair séries do SGS do BACEN
  series_name <- paste0("sgs::", codigo, "::", freq)
  
  retry_fun <- function(x) httr::RETRY(
    "GET",
    x,
    times = 3,
    pause_base = 2,
    pause_cap = 30,
    pause_min = 1
  )
  
  ## Série não diária (faixa completa)
  if (freq != "D") {
    url <- sprintf(
      "https://api.bcb.gov.br/dados/serie/bcdata.sgs.%s/dados?formato=json",
      codigo
    )
    
    response <- retry_fun(url)
    if (httr::http_error(response)) {
      warning("sgs(): erro HTTP ao buscar série não-diária: ", codigo)
      return(NULL)
    }
    
    cont <- httr::content(response, as = "parsed", type = "application/json")
    if (length(cont) == 0) return(NULL)
    
    dados <- lapply(cont, function(x) {
      list(
        data = as.Date(x$data, format = "%d/%m/%Y"),
        valor = as.numeric(x$valor)
      )
    })
    dados <- do.call(rbind, lapply(dados, as.data.frame))
    dados <- dplyr::arrange(dados, data)
    row.names(dados) <- NULL
    
    return(dados)
  }
  
  ## Série diária (busca em blocos retroativos)
  # Define um início de busca razoável se não informado
  if (!is.null(inicio_diario)) {
    inicio_diario <- as.Date(inicio_diario)
  } else {
    # Se não informado, buscar desde 100 anos atrás (ajuste se preferir)
    inicio_diario <- Sys.Date() - lubridate::years(100)
  }
  
  dados_list <- list()
  data_final <- Sys.Date()
  # primeiro bloco: últimos 10 anos até hoje (inclusive)
  data_inicial <- max(inicio_diario, data_final - lubridate::years(10) + 1)
  
  while (TRUE) {
    url <- sprintf(
      "https://api.bcb.gov.br/dados/serie/bcdata.sgs.%s/dados?formato=json&dataInicial=%s&dataFinal=%s",
      codigo,
      format(data_inicial, "%d/%m/%Y"),
      format(data_final, "%d/%m/%Y")
    )
    
    response <- retry_fun(url)
    if (httr::http_error(response)) {
      warning("sgs(): erro HTTP ao buscar período: ", url)
      break
    }
    
    res <- httr::content(response, as = "parsed", type = "application/json")
    # se não houver dados retornados neste bloco, interrompe
    if (length(res) == 0) break
    
    dados_list <- c(dados_list, res)
    
    # se já alcançamos o início desejado, paramos
    if (data_inicial <= inicio_diario) break
    
    # retrocede o período: o próximo bloco termina um dia antes do data_inicial atual
    data_final <- data_inicial - 1
    data_inicial <- max(inicio_diario, data_final - lubridate::years(10) + 1)
  }
  
  if (length(dados_list) == 0) {
    return(NULL)
  }
  
  # Filtra itens sem valor e converte para data.frame
  dados_list <- Filter(function(x) !is.null(x$valor) && nzchar(as.character(x$valor)), dados_list)
  if (length(dados_list) == 0) return(NULL)
  
  dados <- lapply(dados_list, function(x) {
    list(data = as.Date(x$data, format = "%d/%m/%Y"),
         valor = as.numeric(x$valor))
  })
  dados <- do.call(rbind, lapply(dados, as.data.frame))
  dados <- dplyr::arrange(dados, data)
  row.names(dados) <- NULL
  
  return(dados)
}