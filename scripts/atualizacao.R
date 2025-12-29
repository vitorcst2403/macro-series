# Função para obter a informação mais atual da série conforme o método especificado

atual_serie <- function(regra, ...) {
  # Atualiza série com base na regra de formação dependendo do método (class)
  UseMethod("atual_serie")
}

atual_serie.default <- function(regra, ...) {
  stop("Método inválido para esse objeto.")
}

# Métodos primários --------------------------------------------

# Métodos de consulta aos dados originais da WEB

## SNIPC --------------------------------------------

atual_serie.snipc_puro <- function(regra, ...) {
  # Método para extração de série do SNIPC
  
  meta <- regra$meta
  
  cod <- regra$dados$codigo
  tab <- regra$dados$tabela
  
  terr <- meta$territorio
  freq <- meta$frequencia
  medi <- meta$medida
  
  dados <- snipc(
    tabela = tab,
    codigo = cod,
    variavel = medi,
    territorio = terr
  )
  
  if (is.null(dados)) {
    return(NULL)
  }
  
  ms <- ms_dados(dados = dados, regra = regra)
  
  return(ms)
}

## SGS --------------------------------------------

# cria série do SGS com base na regra
atual_serie.sgs_puro <- function(regra, periodo_inicial, ...) {
  codigo <- regra$dados$codigo
  freq <- regra$dados$frequencia
  
  inicio_diario = NULL
  if (freq == "D") {
    inicio_diario <- periodo_inicial
  }
  
  dados <- sgs(
    codigo = codigo,
    freq = freq,
    inicio_diario = inicio_diario
  )
  
  if (is.null(dados)) {
    return(NULL)
  }
  
  ms <- ms_dados(dados = dados, 
                 regra = regra)
  
  return(ms)
}

# Métodos recursivos ---------------------------------------

# Métodos que combinam séries entre si

## Séries combinadas ----------------------------------------------------------

atual_serie.series_resume <- function(regra, periodo_inicial, ...) {
  # Resume uma coleção de séries com base numa função
  
  meta <- regra$meta
  dados <- regra$dados
  
  freq <- meta$frequencia
  inicio <- meta$inicio
  
  periodo_inicial <- as.Date(max(periodo_inicial, inicio))
  
  series <- dados$series
  medidas <- dados$medidas
  frequencias <- dados$frequencias
  territorios <- dados$territorios
  func <- dados$func
  
  if(!is.function(func)) {
    func = eval(parse(text = func))
  }
  
  ms <- coleta_series(
    series = series,
    medidas = medidas,
    frequencias = frequencias,
    territorios = territorios,
    inicio = periodo_inicial
    )
  
  if (is.null(ms)) {
    return(NULL)
  }
  
  ms <- resume_serie(ms, fun = func)
  ms <- janela(ms)
  
  ms$meta <- meta
  
  return(ms)
}

atual_serie.series_aplica <- function(regra, periodo_inicial, ...) {
  # Aplica uma função em uma série
  
  meta <- regra$meta
  dados <- regra$dados
  
  terr <- meta$territorio
  freq <- meta$frequencia
  inicio <- meta$inicio
  
  periodo_inicial <- as.Date(max(periodo_inicial, inicio))
  
  series <- dados$series
  medidas <- dados$medidas
  frequencias <- dados$frequencias
  territorios <- dados$territorios
  func <- dados$func
  larg <- dados$larg
  
  ms <- coleta_series(
    series = series,
    medidas = medidas,
    frequencias = frequencias,
    territorios = territorios,
    inicio = periodo_inicial
  )
  
  if (is.null(ms)) {
    return(NULL)
  }
  
  ms <- aplica_serie(ms, fun = func, width = larg)
  ms <- janela(ms)
  
  ms$meta <- meta
  
  return(ms)
}

## IPCA e IPCA-15 ---------------------------------------------------------------

atual_serie.ipca_composicao <- function(regra, ...) {
  # Método para cálculo de IPCA e IPCA-15 com base na composição de outras séries calculadas
  
  meta <- regra$meta
  dados <- regra$dados
  
  series <- dados$series
  exclusao <- dados$exclusao
  
  medi <- meta$medida
  terr <- meta$territorio
  freq <- meta$frequencia
  
  ms_pesos <- coleta_series(
    series = series,
    medidas = "Peso",
    frequencias = freq,
    territorios = terr,
    inicio = NULL
  )
  
  if (is.null(ms_pesos$serie)) {
    return(NULL)
  }
  
  ms_contribs <- coleta_series(
    series = series,
    medidas = "Contribuição",
    frequencias = freq,
    territorios = terr,
    inicio = NULL
  )
  
  if (is.null(ms_contribs$serie)) {
    return(NULL)
  }
  
  if (!is.null(exclusao)) {
    ms_peso_cheia <- coleta_series(
      series = exclusao,
      medidas = "Peso",
      frequencias = freq,
      territorios = terr,
      inicio = NULL
    )
    
    if (is.null(ms_peso_cheia$serie)) {
      return(NULL)
    }
    
    ms_contrib_cheia <- coleta_series(
      series = exclusao,
      medidas = "Contribuição",
      frequencias = freq,
      territorios = terr,
      inicio = NULL
    )
    
    if (is.null(ms_contrib_cheia$serie)) {
      return(NULL)
    }
  }
  
  contrib <- resume_serie(ms_contribs, fun = sum)
  peso <- resume_serie(ms_pesos, fun = sum)
  
  if (!is.null(exclusao)) {
    contrib <- combina_serie(ms_contrib_cheia, contrib)
    temp_fun <- function(x) {
      return(x[1]-x[2])
    }
    contrib <- resume_serie(contrib, fun = temp_fun)
    
    peso <- combina_serie(ms_peso_cheia, peso)
    peso <- resume_serie(peso, fun = temp_fun)
  }
  
  if (medi == "Peso") {
    peso$meta <- meta
    
    return(peso)
  }
  
  ms <- combina_serie(contrib, peso)
  temp_fun <- function(x) {
    return(x[1]/x[2]*100)
  }
  ms <- resume_serie(ms, fun = temp_fun)
  ms <- janela(ms)
  ms$meta <- meta
  
  return(ms)
}

atual_serie.ipca_nucleo <- function(regra, ...) {
  # Método para calcular os núcleos do IPCA e IPCA-15
  
  meta <- regra$meta
  dados <- regra$dados
  
  nucleo <- dados$nucleo
  series <- dados$series
  suav <- dados$suav
  serie_cheia <- dados$serie_cheia
  
  medi <- meta$medida
  terr <- meta$territorio
  freq <- meta$frequencia
  
  # extrai séries de variação mensal e pesos
  ms_pesos <- coleta_series(
    series = series,
    medidas = "Peso",
    frequencias = freq,
    territorios = terr,
    inicio = NULL
  )
  
  if (is.null(ms_pesos)) {
    return(NULL)
  }
  
  if (nucleo != "ms") {
    ms_cheia <- coleta_series(
      series = serie_cheia,
      medidas = "Variação mensal",
      frequencias = freq,
      territorios = terr,
      inicio = NULL
    )
    
    if (is.null(ms_cheia)) {
      return(NULL)
    }
  }
  
  if (!is.null(suav)) {
    series_totais <- series
    series <- setdiff(series, suav)
  }
  
  ms_vars <- coleta_series(
    series = series,
    medidas = "Variação mensal",
    frequencias = freq,
    territorios = terr,
    inicio = NULL
  )
  
  if (is.null(ms_vars)) {
    return(NULL)
  }
  
  if (!is.null(suav)) {
    ms_suav <- coleta_series(
      series = suav,
      medidas = "Variação mensal",
      frequencias = freq,
      territorios = terr,
      inicio = NULL
    )
    
    if (is.null(ms_suav)) {
      return(NULL)
    }
    
    temp_fun <- function(x) {
      (exp(mean(log(1 + x / 100))) - 1) * 100
    }
    
    ms_suav <- aplica_serie(
      ms_suav,
      fun = temp_fun,
      width = 12,
      partial = TRUE
    )
    
    ms_vars <- combina_serie(ms_vars, ms_suav, fill = FALSE)
    
    temp_fun <- function(x, series, suav, series_totais) {
      n_total <- length(series_totais)
      vec <- numeric(length = n_total)
      
      n_series <- length(series)
      vec[!(series_totais %in% suav)] <- x[1:n_series]
      vec[series_totais %in% suav] <- x[(n_series + 1):n_total]
      
      return(vec)
    }
    
    ms_vars <- resume_serie(
      ms_vars,
      fun = temp_fun,
      series = series,
      suav = suav,
      series_totais = series_totais
    )
  }
  
  if (!is.null(suav)) {
    series <- series_totais
  }
  
  ms_peso_cheia <- coleta_series(
    series = serie_cheia,
    medidas = "Peso",
    frequencias = freq,
    territorios = terr,
    inicio = NULL
  )
  
  if (is.null(ms_peso_cheia)) {
    return(NULL)
  }
  
  comb_pesos <- combina_serie(x1 = ms_peso_cheia, x2 = ms_pesos)
  temp_fun <- function(x) {
    return(x[-1]/x[1]*100)
  }
  
  ms_pesos <- resume_serie(x = comb_pesos, fun = temp_fun)
  
  if (nucleo == "ms") {
    order_vars <- resume_serie(ms_vars, fun = order)
    
    temp_fun <- function(x) {
      order <- x[(length(x)/2+1):length(x)]
      pesos <- x[1:(length(x)/2)]
      return(pesos[order])
    }
    
    ms_pesos_ordem <- resume_serie(
      combina_serie(x1 = ms_pesos, x2 = order_vars),
      fun = temp_fun)
    
    temp_fun <- function(x) {
      order <- x[(length(x)/2+1):length(x)]
      vars <- x[1:(length(x)/2)]
      return(vars[order])
    }
    
    ms_vars_ordem <- resume_serie(
      combina_serie(x1 = ms_vars, x2 = order_vars),
      fun = temp_fun)
    
    ms_pesos_acum <- resume_serie(ms_pesos_ordem, fun = cumsum)
    
    temp_fun <- function(x) {
      c(which(x-20 > 0)[1],
        x[which(x-20 > 0)[1]]-20,
        which(x-80 > 0)[1],
        80-x[which(x-80 > 0)[1]-1])
    }
    
    ms_posicoes <- resume_serie(
      x = ms_pesos_acum,
      fun = temp_fun)
    n = length(series)
    ms_combs <- combina_serie(x1 = combina_serie(x1 = ms_pesos_ordem, x2 = ms_vars_ordem),
                              x2 = ms_posicoes)
    
    temp_fun <- function(x, n) {
      pesos <- x[1:n]
      vars <- x[(n+1):(2*n)]
      posi <- x[(2*n+1):length(x)]
      subset_p <- pesos[posi[1]:posi[3]]
      subset_p[1] <- posi[2]
      subset_p[length(subset_p)] <- posi[4]
      subset_v <- vars[posi[1]:posi[3]]
      value <- sum(subset_p*subset_v)/sum(subset_p)
      return(value)
    }
    
    ms <- resume_serie(
      ms_combs,
      fun = temp_fun,
      n = n)
    ms$meta <- meta
    ms <- janela(ms)
    
    return(ms)
  }
  
  if (nucleo == "dp") {
    ms_total <- combina_serie(ms_cheia, ms_vars)
    temp_fun <- function(x) x[-1]-x[1]
    dif_vars <- resume_serie(ms_total, fun = temp_fun)
    ms_sds <- aplica_serie(
      dif_vars,
      fun = sd,
      width = 48,
      partial = TRUE
    )
    comb_pesos <- combina_serie(x1 = ms_pesos, x2 = ms_sds)
    temp_fun <- function(x, ncols) x[1:ncols]/x[(ncols+1):(2*ncols)]
    ms_pesos_ajus <- resume_serie(x = comb_pesos,
                                  fun = temp_fun,
                                  ncols = ncol(ms_sds))
    comb_series <- combina_serie(x1 = ms_vars, x2 = ms_pesos_ajus)
    temp_fun <- function(x, ncols) {
      sum(x[1:ncols]*x[(ncols+1):(2*ncols)])/sum(x[(ncols+1):(2*ncols)])
    }
    ms <- resume_serie(x = comb_series,
                       fun = temp_fun,
                       ncols = ncol(ms_vars))
    ms$meta <- meta
    ms <- janela(ms)
    
    return(ms)
  }
  
  if (nucleo == "p55") {
    order_vars <- resume_serie(ms_vars, fun = order)
    comb_series <- combina_serie(x1 = combina_serie(x1 = ms_pesos, x2 = order_vars),
                                 x2 = ms_vars)
    n = length(series)
    temp_fun <- function(x, n) {
      x1 <- x[1:n]
      x2 <- x[(n+1):(2*n)]
      x3 <- x[(2*n+1):(3*n)]
      posi <- which(cumsum(x1[x2])-55 > 0)[1]
      return(sort(x3)[posi])
    }
    ms <- resume_serie(
      x = comb_series,
      fun = temp_fun,
      n = n
    )
    ms$meta <- meta
    ms <- janela(ms)
    
    return(ms)
  }
}

