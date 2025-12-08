# Obtem séries --------------------------------------------
obtem_series <- function(series,
                         medidas = NULL,
                         frequencias = NULL,
                         territorios = NULL,
                         inicio = NULL,
                         ignora_nulo = TRUE,
                         combina = TRUE) {
  # Essa função extrai as séries do repositório ou monta com base em dados da web
  
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

  ## Extrair série -----------------------------------------------------
  
  extrai_serie <- function(serie,
                           medida,
                           frequencia,
                           territorio,
                           inicio) {
    # função para extrair uma única série
    
    ### Atualizar série ------------------
    
    # Função para obter a informação mais atual da série conforme o método especificado
    
    atual_serie <- function(regra, ...) {
      # Atualiza série com base na regra de formação dependendo do método (class)
      UseMethod("atual_serie")
    }
    
    atual_serie.default <- function(regra, ...) {
      stop("Método inválido para esse objeto.")
    }
    
    #### Métodos primários --------------------------------------------
    
    # Métodos de consulta aos dados originais da WEB
    
    ##### SNIPC --------------------------------------------
    
    atual_serie.snipc_puro <- function(regra, ...) {
      # Método para extração de série do SNIPC
      
      cod <- regra$codigo
      tab <- regra$tabela
      terr <- rec_territorio(regra)
      freq <- rec_frequencia(regra)
      medi <- rec_medida(regra)
      
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
    
    ##### SGS --------------------------------------------
    
    # cria série do SGS com base na regra
    atual_serie.sgs_puro <- function(regra, periodo_inicial) {
      codigo <- regra$codigo
      freq <- rec_frequencia(regra)
      
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
      
      ms <- ms_dados(dados = dados, regra = regra)
      
      return(ms)
    }
    
    #### Métodos recursivos ---------------------------------------
    
    # Métodos que combinam séries entre si
    
    ##### Séries combinadas ----------------------------------------------------------
    
    atual_serie.series_resume <- function(regra, periodo_inicial) {
      # Resume uma coleção de séries com base numa função
      
      freq <- rec_frequencia(regra)
      inicio <- rec_inicio(regra)
      
      periodo_inicial <- as.Date(max(periodo_inicial, inicio))
      
      series <- regra$series
      medidas <- regra$medidas
      frequencias <- regra$frequencias
      territorios <- regra$territorios
      func <- regra$func
      
      ms <- obtem_series(
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
      
      ms <- attr_regra(ms, regra)
      
      return(ms)
    }
    
    atual_serie.series_aplica <- function(regra, periodo_inicial) {
      # Aplica uma função em uma série
      
      terr <- rec_territorio(regra)
      freq <- rec_frequencia(regra)
      inicio <- rec_inicio(regra)
      
      periodo_inicial <- as.Date(max(periodo_inicial, inicio))
      
      series <- regra$series
      medidas <- regra$medidas
      frequencias <- regra$frequencias
      territorios <- regra$territorios
      func <- regra$func
      larg <- regra$larg
      
      ms <- obtem_series(
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
      
      return(ms)
    }
    
    ##### IPCA e IPCA-15 ---------------------------------------------------------------
    
    atual_serie.ipca_composicao <- function(regra, ...) {
      # Método para cálculo de IPCA e IPCA-15 com base na composição de outras séries calculadas
      
      series <- regra$series
      exclusao <- regra$exclusao
      
      medi <- rec_medida(regra)
      terr <- rec_territorio(regra)
      freq <- rec_frequencia(regra)
      
      ms_pesos <- obtem_series(
        series = series,
        medidas = "Peso",
        frequencias = freq,
        territorios = terr,
        inicio = NULL
      )
      
      if (is.null(ms_pesos)) {
        return(NULL)
      }
      
      ms_contribs <- obtem_series(
        series = series,
        medidas = "Contribuição",
        frequencias = freq,
        territorios = terr,
        inicio = NULL
      )
      
      if (is.null(ms_contribs)) {
        return(NULL)
      }
      
      if (!is.null(exclusao)) {
        ms_peso_cheia <- obtem_series(
          series = exclusao,
          medidas = "Peso",
          frequencias = freq,
          territorios = terr,
          inicio = NULL
        )
        
        if (is.null(ms_peso_cheia)) {
          return(NULL)
        }
        
        ms_contrib_cheia <- obtem_series(
          series = exclusao,
          medidas = "Contribuição",
          frequencias = freq,
          territorios = terr,
          inicio = NULL
        )
        
        if (is.null(ms_contrib_cheia)) {
          return(NULL)
        }
      }
      
      contrib <- resume_serie(ms_contribs, fun = "sum(x)")
      peso <- resume_serie(ms_pesos, fun = "sum(x)")
      
      if (!is.null(exclusao)) {
        contrib <- combina_serie(ms_contrib_cheia, contrib)
        contrib <- resume_serie(contrib, fun = "x[1]-x[2]")
        
        peso <- combina_serie(ms_peso_cheia, peso)
        peso <- resume_serie(peso, fun = "x[1]-x[2]")
      }
      
      if (medi == "Peso") {
        peso <- attr_regra(peso, regra)
        
        return(peso)
      }
      
      ms <- combina_serie(contrib, peso)
      ms <- resume_serie(ms, fun = "x[1]/x[2]*100")
      ms <- janela(ms)
      ms <- attr_regra(ms, regra)
      
      return(ms)
    }
    
    atual_serie.ipca_nucleo <- function(regra, ...) {
      # Método para calcular os núcleos do IPCA e IPCA-15
      
      nucleo <- regra$nucleo
      series <- regra$series
      suav <- regra$suav
      serie_cheia <- regra$serie_cheia
      
      medi <- rec_medida(regra)
      terr <- rec_territorio(regra)
      freq <- rec_frequencia(regra)
      
      # extrai séries de variação mensal e pesos
      ms_pesos <- obtem_series(
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
        ms_cheia <- obtem_series(
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
      
      ms_vars <- obtem_series(
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
        ms_suav <- obtem_series(
          series = suav,
          medidas = "Variação mensal",
          frequencias = freq,
          territorios = terr,
          inicio = NULL
        )
        
        if (is.null(ms_suav)) {
          return(NULL)
        }
        
        ms_suav <- aplica_serie(
          ms_suav,
          fun = function(x)
            (exp(mean(
              log(1 + x / 100)
            )) - 1) * 100,
          width = 12,
          partial = TRUE
        )
        
        ms_vars <- combina_serie(ms_vars, ms_suav, fill = FALSE)
        ms_vars <- resume_serie(
          ms_vars,
          fun = "vec <- numeric(length = ncol(ms_vars))\n
                                  vec[!(series_totais %in% suav)] <- x[1:length(series)]\n
                                  vec[series_totais %in% suav] <- x[(length(series)+1):length(series_totais)]\n
                                  return(vec)",
          series = series,
          suav = suav,
          series_totais = series_totais
        )
      }
      
      tsp_vars <- tsp(ms_vars)
      inicio_vars <- rec_inicio(ms_vars)
      fim_vars <- rec_fim(ms_vars)
      
      if (!is.null(suav)) {
        series <- series_totais
      }
      
      ms_peso_cheia <- obtem_series(
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
      ms_pesos <- resume_serie(x = comb_pesos, fun = "x[-1]/x[1]*100")
      
      if (nucleo == "ms") {
        order_vars <- resume_serie(ms_vars, fun = "order(x)")
        
        ms_pesos_ordem <- resume_serie(
          combina_serie(x1 = ms_pesos, x2 = order_vars),
          fun = "order <- x[(length(x)/2+1):length(x)]\n
                                         pesos <- x[1:(length(x)/2)]\n
                                         return(pesos[order])"
        )
        ms_vars_ordem <- resume_serie(
          combina_serie(x1 = ms_vars, x2 = order_vars),
          fun = "order <- x[(length(x)/2+1):length(x)]\n
                                         vars <- x[1:(length(x)/2)]\n
                                         return(vars[order])"
        )
        ms_pesos_acum <- resume_serie(ms_pesos_ordem, fun = "cumsum(x)")
        ms_posicoes <- resume_serie(
          x = ms_pesos_acum,
          fun = "c(which(x-20 > 0)[1],
                                    x[which(x-20 > 0)[1]]-20,
                                    which(x-80 > 0)[1],
                                    80-x[which(x-80 > 0)[1]-1])"
        )
        n = length(series)
        ms_combs <- combina_serie(x1 = combina_serie(x1 = ms_pesos_ordem, x2 = ms_vars_ordem),
                                  x2 = ms_posicoes)
        
        ms <- resume_serie(
          ms_combs,
          fun = "pesos <- x[1:n]\n
                                        vars <- x[(n+1):(2*n)]\n
                                        posi <- x[(2*n+1):length(x)]\n
                                        subset_p <- pesos[posi[1]:posi[3]]\n
                                        subset_p[1] <- posi[2]\n
                                        subset_p[length(subset_p)] <- posi[4]\n
                                        subset_v <- vars[posi[1]:posi[3]]\n
                                        value <- sum(subset_p*subset_v)/sum(subset_p)\n
                                        return(value)",
          n = n
        )
        ms <- attr_regra(ms, regra)
        ms <- janela(ms)
        
        return(ms)
      }
      
      if (nucleo == "dp") {
        ms_total <- combina_serie(ms_cheia, ms_vars)
        dif_vars <- resume_serie(ms_total, fun = "x[-1]-x[1]")
        ms_sds <- aplica_serie(
          dif_vars,
          fun = sd,
          width = 48,
          partial = TRUE
        )
        comb_pesos <- combina_serie(x1 = ms_pesos, x2 = ms_sds)
        ms_pesos_ajus <- resume_serie(x = comb_pesos,
                                      fun = "x[1:ncols]/x[(ncols+1):(2*ncols)]",
                                      ncols = ncol(ms_sds))
        comb_series <- combina_serie(x1 = ms_vars, x2 = ms_pesos_ajus)
        ms <- resume_serie(x = comb_series,
                           fun = "sum(x[1:ncols]*x[(ncols+1):(2*ncols)])/sum(x[(ncols+1):(2*ncols)])",
                           ncols = ncol(ms_vars))
        ms <- attr_regra(ms, regra)
        ms <- janela(ms)
        
        return(ms)
      }
      
      if (nucleo == "p55") {
        order_vars <- resume_serie(ms_vars, fun = "order(x)")
        comb_series <- combina_serie(x1 = combina_serie(x1 = ms_pesos, x2 = order_vars),
                                     x2 = ms_vars)
        n = length(series)
        ms <- resume_serie(
          x = comb_series,
          fun = "x1 <- x[1:n]\n
                                         x2 <- x[(n+1):(2*n)]\n
                                         x3 <- x[(2*n+1):(3*n)]\n
                                         posi <- which(cumsum(x1[x2])-55 > 0)[1]\n
                                         return(sort(x3)[posi])",
          n = n
        )
        ms <- attr_regra(ms, regra)
        ms <- janela(ms)
        
        return(ms)
      }
    }
    
    ### Encontra regra e extrai --------------------------------------------------
    
    atual <- getOption("macroseries.atualiza")
    
    if(is.null(atual)) {
      message("Opção 'macroserie.atualiza' não definida. Default é FALSE. Defina com a função opcao_atualiza().")
      atual = FALSE
    }
    
    if (!is.character(serie[1])) {
      stop("Série deve ser um vetor de caracteres")
    }
    
    regra <- class_regra(
      serie,
      medida = medida,
      frequencia = frequencia,
      territorio = territorio
    )
    
    inicio_regra <- rec_inicio(regra)
    
    if (is.null(regra)) {
      message("Sem regra de formação para a combinação escolhida.")
      return(NULL)
    }
    
    ms <- extrai_repo(regra)
    
    if (atual) {
      if (!exists("ms", envir = environment(), inherits = FALSE) ||
          is.null(ms)) {
        ms <- atual_serie(
          regra = regra,
          periodo_inicial = inicio_regra
        )
        
        if (is.null(ms)) {
          message(paste(
            "Não foi encontrada atualização da série",
            rec_serie(regra)
          ))
          return(NULL)
        }
        
        salva_repo(ms, regra)
        
        return(ms)
      } else {
        if (is.null(inicio)) {
          inicio <- inicio_regra
        }
        
        ms_atual <- atual_serie(
          regra = regra,
          periodo_inicial = inicio
        )
        
        if (!is.null(ms_atual)) {
          if (rec_fim(ms_atual) > rec_fim(ms)) {
            ms <- combina_serie(ms, ms_atual, fill = FALSE)
            ms <- resume_serie(ms, fun = "aglut(x)")
            salva_repo(ms, regra)
            
            return(ms)
          }
        }
      }
    }
    
    if(is.null(ms)) {
      message("Série não encontrada. Atualize com 'atualiza = TRUE'.")
    }
    
    return(ms)
  }
  
  ## Obtem lista de séries ---------------------------------------
  
  ms_series <- mapply(
    function(serie,
             medida,
             frequencia,
             territorio,
             inicio) {
      extrai_serie(serie,
                   medida,
                   frequencia,
                   territorio,
                   inicio)
    },
    series,
    medidas,
    frequencias,
    territorios,
    inicios,
    SIMPLIFY = FALSE
  )
  
  if (!ignora_nulo && sum(sapply(ms_series, is.null)) > 0) {
    return(NULL)
  }
  
  if (sum(sapply(ms_series, is.null)) == length(ms_series)) {
    return(NULL)
  }
  
  ms_series <- Filter(function(x)
    ! is.null(x), ms_series)
  
  if (combina) {
    ms <- Reduce(function(x, y)
      combina_serie(x, y, fill = FALSE), ms_series)
  } else {
    return(ms_series)
  }
  
  return(ms)
}

opcao_atualiza <- function(opcao = FALSE) {
  if(!is.logical(opcao)) {
    stop("opcao deve ser lógico.")
  }
  
  options(macroseries.atualiza = opcao)
}
