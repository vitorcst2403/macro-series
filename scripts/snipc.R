# SNIPC -------------------------------------------------------------------

snipc <- function(tabela,
                  codigo,
                  variavel,
                  territorio) {
  # função para consulta no API do SNIPC(IBGE)
  retry_fun <- function(x) httr::RETRY("GET", x, 
                                       times = 3, 
                                       pause_base = 2, 
                                       pause_cap = 30, 
                                       pause_min = 1)
  
  tabelas <- c(
    "7060",
    "7061",
    "7062",
    "7063"
  )
  
  if(!(tabela %in% tabelas)) {
    stop("Tabela inválida.")
  }
  
  if(!(variavel %in% c("Variação mensal", "Peso"))) {
    stop("Variável inválida.")
  }
  
  variaveis <- list(
    "7060" = c("Variação mensal" = "63", "Peso" = "66"),
    "7061" = c("Variação mensal" = "306","Peso" =  "309"),
    "7062" = c("Variação mensal" = "355", "Peso" = "357"),
    "7063" = c("Variação mensal" = "44", "Peso" = "45")
  )
  
  var <- variaveis[[tabela]][variavel]
  if (variavel == "Variação mensal") {
    dig <- paste0("v", var, "%20", 2)
  } else {
    dig <- paste0("v", var, "%20", 4)
  }
  
  territorio_nome <- c(
    "Brasil",
    "Belém - PA",
    "Fortaleza - CE",
    "Recife - PE",
    "Salvador - BA",
    "Belo Horizonte - MG",
    "Grande Vitória - ES",
    "Rio de Janeiro - RJ",
    "São Paulo - SP",
    "Curitiba - PR",
    "Porto Alegre - RS",
    "Rio Branco - AC",
    "São Luís - MA",
    "Aracaju - SE",
    "Campo Grande - MS",
    "Goiânia - GO",
    "Brasília - DF"
  )
  
  if(!(territorio %in% territorio_nome)) {
    stop("Território inválido.")
  }
  
  territorios <- c(
    "all",
    "1501",
    "2301",
    "2601",
    "2901",
    "3101",
    "3201",
    "3301",
    "3501",
    "4101",
    "4301",
    "1200401",
    "2111300",
    "2800308",
    "5002704",
    "5208707",
    "5300108"
  )
  names(territorios) <- territorio_nome
  terr <- territorios[territorio]
  
  cod = codigo
  tab = tabela
  
  table_name <- paste(c("snipc",
                        tab, 
                        cod,
                        var,
                        terr), collapse = "_")
  
  t_numero = NULL
  
  if(territorio == "Brasil") {
    t_numero = "n1"
  } 
  
  if(territorio %in% c("Rio Branco - AC",
                       "São Luís - MA",
                       "Aracaju - SE",
                       "Campo Grande - MS",
                       "Goiânia - GO",
                       "Brasília - DF")) {
    t_numero = "n6"
  }
  
  if(is.null(t_numero)) {
    t_numero = "n7"
  }
  
  base_url <- "https://apisidra.ibge.gov.br/values"
  url <- paste(
    c(base_url,
      "t",
      tab,
      t_numero,
      terr,
      "v",
      var,
      "p",
      "all",
      "c315",
      cod,
      "d",
      dig
    ),
    collapse = "/"
  )
  
  res <- retry_fun(url)
  error <- httr::http_error(res)
  
  if(error) {
    return(NULL)
  }
  
  cont <- httr::content(res, as = "parsed", type = "application/json")
  cont <- purrr::transpose(cont)
  cont <- as.data.frame(lapply(cont, unlist))
  dados <- cont[-1,]
  names(dados) <- cont[1,]
  dados <- dplyr::select(dados, `Mês (Código)`, `Valor`)
  names(dados) <- c("data", "valor")
  dados <- dplyr::mutate(
    dados,
    data = as.Date(paste0(substr(data, 1, 4), "-", substr(data, 5, 6), "-01")),
    valor = as.numeric(valor)
  )
  
  return(dados)
}

