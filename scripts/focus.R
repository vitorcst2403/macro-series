# Focus por data de referência --------------------------------------------
focus_referencia <- function(referencia,
                             indicador,
                             relatorio,
                             estatistica,
                             base,
                             suavizado,
                             tipo_calculo) {
  encoded_indicadores <- c(
    `IPCA` = "IPCA",
    `IPCA Administrados` = "IPCA%20Administrados",
    `IPCA Alimentação no domicílio` = "IPCA%20Alimenta%C3%A7%C3%A3o%20no%20domic%C3%ADlio",
    `IPCA Bens industriais` = "IPCA%20Bens%20industrializados",
    `IPCA Livres` = "IPCA%20Livres",
    `IPCA Serviços` = "IPCA%20Servi%C3%A7os",
    `IPCA-15` = "IPCA-15",
    `IGP-M` = "IGP-M",
    `IGP-DI` = "IGP-DI",
    `IPA-DI` = "IPA-DI",
    `IPA-M` = "IPA-M",
    `IPC-Fipe` = "IPC-Fipe",
    `Taxa de Câmbio` = "C%C3%A2mbio",
    `Selic` ="Selic",
    `PIB` = "PIB%20Total",
    `PIB Industría` = "PIB%20Ind%C3%BAstria",
    `PIB Exportações` = "PIB%20Exporta%C3%A7%C3%A3o%20de%20bens%20e%20servi%C3%A7os",
    `PIB Agropecuária` = "PIB%20Agropecu%C3%A1ria",
    `PIB Serviços` = "PIB%20Servi%C3%A7os",
    `PIB Consumo das Famílias` = "PIB%20Despesa%20de%20consumo%20das%20fam%C3%ADlias",
    `PIB Formação Bruta` = "PIB%20Forma%C3%A7%C3%A3o%20Bruta%20de%20Capital%20Fixo",
    `PIB Importação` = "PIB%20Importa%C3%A7%C3%A3o%20de%20bens%20e%20servi%C3%A7os",
    `PIB Administração Pública` = "PIB%20Despesa%20de%20consumo%20da%20administra%C3%A7%C3%A3o%20p%C3%BAblica",
    `Desocupação` = "Taxa%20de%20desocupa%C3%A7%C3%A3o",
    `Produção industrial` = "Produ%C3%A7%C3%A3o%20industrial",
    `Dívida Bruta do Governo Geral` = "D%C3%ADvida%20bruta%20do%20governo%20geral",
    `Dívida Líquida do Setor Público` = "D%C3%ADvida%20l%C3%ADquida%20do%20setor%20p%C3%BAblico",
    `Resultado Nominal` = "Resultado%20nominal",
    `Resultado Primário` = "Resultado%20prim%C3%A1rio",
    `Conta Corrente` = "Conta%20corrente",
    `Balança Comercial - Exportações` = "Balan%C3%A7a%20comercial",
    `Balança Comercial - Importações` = "Balan%C3%A7a%20comercial",
    `Balança Comercial - Saldo` = "Balan%C3%A7a%20comercial",
    `Investimento Direto no País` = "Investimento%20direto%20no%20pa%C3%ADs"
  )
  
  encoded_detalhe <- c(
    `Balança Comercial - Exportações` = "Exporta%C3%A7%C3%B5es",
    `Balança Comercial - Importações` = "Importa%C3%A7%C3%B5es",
    `Balança Comercial - Saldo` = "Saldo"
  )
  
  encoded_relatorios <- c(
    `Mensal` = "ExpectativaMercadoMensais",
    `Top5 Mensal` = "ExpectativasMercadoTop5Mensais",
    `Trimestral` = "ExpectativasMercadoTrimestrais",
    `Top5 Trimestral` = "ExpectativasMercadoTop5Trimestrais",
    `Anual` = "ExpectativasMercadoAnuais",
    `Top5 Anual` = "ExpectativasMercadoTop5Anuais",
    `Horizontes Móveis 12 Meses` = "ExpectativasMercadoInflacao12Meses",
    `Top 5 Horizontes Móveis 12 Meses` = "ExpectativasMercadoTop5Inflacao12Meses",
    `Horizontes Móveis 24 Meses` = "ExpectativasMercadoInflacao24Meses",
    `Top 5 Horizontes Móveis 24 Meses` = "ExpectativasMercadoTop5Inflacao24Meses",
    `Selic` = "ExpectativasMercadoSelic",
    `Top5 Selic` = "ExpectativasMercadoTop5Selic",
    `Datas de Referência` = "DatasReferencia"
  )
  
  encoded_estatisticas <- c(
    `Média` = "Media",
    `Mediana` = "Mediana",
    `Desvio Padrão` = "DesvioPadrao",
    `Coeficiente de Variação` = "coeficienteVariacao",
    `Mínimo` = "Minimo",
    `Máximo` = "Maximo",
    `Número de Respondentes` = "numeroRespondentes"
  )
  
  encoded_referencia <- c(
    `Mensal` = "DataReferencia",
    `Top5 Mensal` = "DataReferencia",
    `Trimestral` = "DataReferencia",
    `Top5 Trimestral` = "DataReferencia",
    `Anual` = "DataReferencia",
    `Top5 Anual` = "DataReferencia",
    `Selic` = "Reuniao",
    `Top5 Selic` = "Reuniao"
  )
  
  retry_fun <- function(x) httr::RETRY(
    "GET",
    x,
    times = 3,
    pause_base = 2,
    pause_cap = 30,
    pause_min = 1
  )
  
  ## Base e relatório
  
  base_url <- "https://olinda.bcb.gov.br/olinda/servico/Expectativas/versao/v1/odata"
  
  relatorio_url <- encoded_relatorios[relatorio]
  if(is.na(relatorio) || length(relatorio) == 0) {
    warning("Relatório inválido.")
    return(NULL)
  }
  
  url <- paste0(base_url, "/", relatorio_url, "?$top=10000")
  
  ## Filtros
  
  indicador <- encoded_indicadores[indicador]
  if(is.na(indicador) || length(indicador) == 0) {
    warning("Indicador inválido.")
    return(NULL)
  }
  filtros <- paste0("Indicador%20eq%20'", indicador, "'")
  
  detalhe <- encoded_detalhe[indicador]
  if(!is.na(detalhe)) {
    filtros <- paste0(indicador, "%20and%20IndicadorDetalhe%20eq%20'", detalhe, "'")
  }
  
  filtros <- paste0(filtros, "%20and%20baseCalculo%20eq%20", base)
  
  if(!is.null(referencia)) {
    pre_referencia <- encoded_referencia[relatorio]
    if(is.na(pre_referencia)) pre_referencia <- NULL
    referencia <- gsub("/", "%2F", referencia)
    filtros <- paste0(filtros, "%20and%20", pre_referencia, "%20eq%20'", referencia, "'")
  }
  
  if (!is.null(suavizado)) {
    filtros <- paste0(filtros, "%20and%20Suavizada%20eq%20'", suavizado, "'")
  }
  
  if (!is.null(tipo_calculo)) {
    filtros <- paste0(filtros, "%20and%tipoCalculo%20eq%20'", tipo_calculo, "'")
  }
  
  url <- paste0(url, "&$filter=", filtros, "&$format=json")
  
  ## Seleção
  
  estatistica <- encoded_estatisticas[estatistica]
  if(is.na(estatistica) || length(estatistica) == 0) {
    warning("Estatística inválida.")
    return(NULL)
  }
  
  url <- paste0(url, "&$select=Data,", estatistica)
  
  # Extração
  
  response <- retry_fun(url)
  if (httr::http_error(response)) {
    warning("sgs(): erro HTTP ao buscar período: ", url)
    break
  }
  
  cont <- httr::content(response, as = "parsed", type = "application/json")
  if (length(cont) == 0) return(NULL)
  
  dados <- lapply(cont$value, function(x) {
    list(
      data = as.Date(x[[1]]),
      valor = as.numeric(x[[2]])
    )
  })
  dados <- do.call(rbind, lapply(dados, as.data.frame))
  dados <- dplyr::arrange(dados, data)
  row.names(dados) <- NULL
  
  return(dados)
}

# Focus por data de relatório ---------------------------------------------
focus_data <- function(data,
                       indicador,
                       relatorio,
                       estatistica,
                       base,
                       suavizado,
                       tipo_calculo) {
  encoded_indicadores <- c(
    `IPCA` = "IPCA",
    `IPCA Administrados` = "IPCA%20Administrados",
    `IPCA Alimentação no domicílio` = "IPCA%20Alimenta%C3%A7%C3%A3o%20no%20domic%C3%ADlio",
    `IPCA Bens industriais` = "IPCA%20Bens%20industrializados",
    `IPCA Livres` = "IPCA%20Livres",
    `IPCA Serviços` = "IPCA%20Servi%C3%A7os",
    `IPCA-15` = "IPCA-15",
    `INPC` = "INPC",
    `IGP-M` = "IGP-M",
    `IGP-DI` = "IGP-DI",
    `IPA-DI` = "IPA-DI",
    `IPA-M` = "IPA-M",
    `IPC-Fipe` = "IPC-Fipe",
    `Taxa de Câmbio` = "C%C3%A2mbio",
    `Selic` ="Selic",
    `PIB` = "PIB%20Total",
    `PIB Indústria` = "PIB%20Ind%C3%BAstria",
    `PIB Exportações` = "PIB%20Exporta%C3%A7%C3%A3o%20de%20bens%20e%20servi%C3%A7os",
    `PIB Agropecuária` = "PIB%20Agropecu%C3%A1ria",
    `PIB Serviços` = "PIB%20Servi%C3%A7os",
    `PIB Consumo das Famílias` = "PIB%20Despesa%20de%20consumo%20das%20fam%C3%ADlias",
    `PIB Formação Bruta` = "PIB%20Forma%C3%A7%C3%A3o%20Bruta%20de%20Capital%20Fixo",
    `PIB Importação` = "PIB%20Importa%C3%A7%C3%A3o%20de%20bens%20e%20servi%C3%A7os",
    `PIB Administração Pública` = "PIB%20Despesa%20de%20consumo%20da%20administra%C3%A7%C3%A3o%20p%C3%BAblica",
    `Desocupação` = "Taxa%20de%20desocupa%C3%A7%C3%A3o",
    `Produção industrial` = "Produ%C3%A7%C3%A3o%20industrial",
    `Dívida Bruta do Governo Geral` = "D%C3%ADvida%20bruta%20do%20governo%20geral",
    `Dívida Líquida do Setor Público` = "D%C3%ADvida%20l%C3%ADquida%20do%20setor%20p%C3%BAblico",
    `Resultado Nominal` = "Resultado%20nominal",
    `Resultado Primário` = "Resultado%20prim%C3%A1rio",
    `Conta Corrente` = "Conta%20corrente",
    `Balança Comercial - Exportações` = "Balan%C3%A7a%20comercial",
    `Balança Comercial - Importações` = "Balan%C3%A7a%20comercial",
    `Balança Comercial - Saldo` = "Balan%C3%A7a%20comercial",
    `Investimento Direto no País` = "Investimento%20direto%20no%20pa%C3%ADs"
  )
  
  encoded_detalhe <- c(
    `Balança Comercial - Exportações` = "Exporta%C3%A7%C3%B5es",
    `Balança Comercial - Importações` = "Importa%C3%A7%C3%B5es",
    `Balança Comercial - Saldo` = "Saldo"
  )
  
  encoded_relatorios <- c(
    `Mensal` = "ExpectativaMercadoMensais",
    `Top5 Mensal` = "ExpectativasMercadoTop5Mensais",
    `Trimestral` = "ExpectativasMercadoTrimestrais",
    `Top5 Trimestral` = "ExpectativasMercadoTop5Trimestrais",
    `Anual` = "ExpectativasMercadoAnuais",
    `Top5 Anual` = "ExpectativasMercadoTop5Anuais",
    `Horizontes Móveis 12 Meses` = "ExpectativasMercadoInflacao12Meses",
    `Top 5 Horizontes Móveis 12 Meses` = "ExpectativasMercadoTop5Inflacao12Meses",
    `Horizontes Móveis 24 Meses` = "ExpectativasMercadoInflacao24Meses",
    `Top 5 Horizontes Móveis 24 Meses` = "ExpectativasMercadoTop5Inflacao24Meses",
    `Selic` = "ExpectativasMercadoSelic",
    `Top5 Selic` = "ExpectativasMercadoTop5Selic",
    `Datas de Referência` = "DatasReferencia"
  )
  
  encoded_estatisticas <- c(
    `Média` = "Media",
    `Mediana` = "Mediana",
    `Desvio Padrão` = "DesvioPadrao",
    `Coeficiente de Variação` = "coeficienteVariacao",
    `Mínimo` = "Minimo",
    `Máximo` = "Maximo",
    `Número de Respondentes` = "numeroRespondentes"
  )
  
  encoded_referencia <- c(
    `Mensal` = "DataReferencia",
    `Top5 Mensal` = "DataReferencia",
    `Trimestral` = "DataReferencia",
    `Top5 Trimestral` = "DataReferencia",
    `Anual` = "DataReferencia",
    `Top5 Anual` = "DataReferencia",
    `Selic` = "Reuniao",
    `Top5 Selic` = "Reuniao"
  )
  
  retry_fun <- function(x) httr::RETRY(
    "GET",
    x,
    times = 3,
    pause_base = 2,
    pause_cap = 30,
    pause_min = 1
  )
  
  ## Base e relatório
  
  base_url <- "https://olinda.bcb.gov.br/olinda/servico/Expectativas/versao/v1/odata"
  
  relatorio_url <- encoded_relatorios[relatorio]
  if(is.na(relatorio) || length(relatorio) == 0) {
    warning("Relatório inválido.")
    return(NULL)
  }
  
  url <- paste0(base_url, "/", relatorio_url, "?$top=10000")
  
  ## Filtros
  
  indicador <- encoded_indicadores[indicador]
  if(is.na(indicador) || length(indicador) == 0) {
    warning("Indicador inválido.")
    return(NULL)
  }
  filtros <- paste0("Indicador%20eq%20'", indicador, "'")
  
  detalhe <- encoded_detalhe[indicador]
  if(!is.na(detalhe)) {
    filtros <- paste0(indicador, "%20and%20IndicadorDetalhe%20eq%20'", detalhe, "'")
  }
  
  filtros <- paste0(filtros, "%20and%20Data%20eq%20'", data, "'")
  
  filtros <- paste0(filtros, "%20and%20baseCalculo%20eq%20", base)
  
  if (!is.null(suavizado)) {
    filtros <- paste0(filtros, "%20and%20Suavizada%20eq%20'", suavizado, "'")
  }
  
  if (!is.null(tipo_calculo)) {
    tipo_calculo = toupper(tipo_calculo)
    filtros <- paste0(filtros, "%20and%tipoCalculo%20eq%20'", tipo_calculo, "'")
  }
  
  url <- paste0(url, "&$filter=", filtros, "&$format=json")
  
  ## Seleção
  
  estatistica <- encoded_estatisticas[estatistica]
  if(is.na(estatistica) || length(estatistica) == 0) {
    warning("Estatística inválida.")
    return(NULL)
  }
  
  referencia <- encoded_referencia[relatorio]
  
  url <- paste0(url, "&$select=", referencia, ",", estatistica)
  
  # Extração
  
  response <- retry_fun(url)
  if (httr::http_error(response)) {
    warning("sgs(): erro HTTP ao buscar período: ", url)
    break
  }
  
  cont <- httr::content(response, as = "parsed", type = "application/json")
  if (length(cont) == 0) return(NULL)
  
  dados <- lapply(cont$value, function(x) {
    list(
      ref = x[[1]],
      valor = as.numeric(x[[2]])
    )
  })
  dados <- do.call(rbind, lapply(dados, as.data.frame))
  dados <- dplyr::arrange(dados, data)
  row.names(dados) <- NULL
  
  return(dados)
}


