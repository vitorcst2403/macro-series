consulta <- function(nome, ..., 
                     inicio = NULL,
                     cache = TRUE,
                     force = FALSE,
                     atual = TRUE,
                     ignora_nulo = TRUE) {
  # função de consulta às macro series
  nomes <- c(...)
  nomes <- c(nome, nomes)
  
  nome_mod <- lapply(strsplit(nomes, "&"), trimws)
  nomes <- sapply(nome_mod, function(x) x[1])
  mods <- sapply(nome_mod, function(x) x[-1])
  mods <- lapply(mods, function(x) {
    if(length(x) == 0) {
      return(NULL)
    } else {
      return(x)
    }
  })
  
  encontra_series <- function(nome) {
    index_fun <- function(nms) {
      return(which(apply(names_tbl[, (ncol(names_tbl)-7):ncol(names_tbl)], 1, function(row) nms %in% row)))
    }
    index <- sapply(nome, index_fun)
    args <- as.list(names_tbl[index,c(1:4, 23)])
    return(args)
  }
  
  args <- c(encontra_series(nomes), list(mods = mods))
  
    # Funções auxiliares -----------------------
    
    ## cria funções que extraem os componentes da regra ou serie
    
    atribs <- c("serie", "unidade", "inicio", "fim", "medida", "frequencia", "territorio", "tema", "descricao")
    funs <- list(inicio = "as.Date", fim = "as.Date")
    
    for(atrib in atribs) {
      wrapper <- if (!is.null(funs[[atrib]])) funs[[atrib]] else NULL
      
      if(!is.null(wrapper)) {
        code <- sprintf(
          'rec_%s <- function(x) { %s(attr(x, "%s")) }',
          atrib, wrapper, atrib
        )
        eval(parse(text = code))
      } else {
        code <- sprintf(
          'rec_%s <- function(x) { attr(x, "%s") }',
          atrib, atrib
        )
        eval(parse(text = code))
      }
    }
    
    gera_datas <- function(freq,
                           inicio,
                           fim) {
    # gera datas entre dois periodos com frequencia pré definida
      bys <- c("D" = "d",
               "M" = "m",
               "T" = "3 m",
               "S" = "6 m",
               "A" = "y")
      
      inicio_ano <- lubridate::year(inicio)
      inicio_mes <- lubridate::month(inicio)
      
      data <- NULL
      
      if(freq == "Q") {
        fim_mes <- lubridate::month(fim)
        fim_ano <- lubridate::year(fim)
        
        data1 <- seq.Date(from = as.Date(paste0(inicio_ano, "-", inicio_mes, "-01")),
                          to = as.Date(paste0(fim_ano, "-", fim_mes, "-01")),
                          by = "m")
        data15 <- seq.Date(from = as.Date(paste0(inicio_ano, "-", inicio_mes, "-15")),
                           to = as.Date(paste0(fim_ano, "-", fim_mes, "-15")),
                           by = "m")
        data <- sort(c(data1, data15))
        data <- data[data >= inicio & data <= fim]
      }
      
      if(is.null(data)) {
        
        data <- seq.Date(from = inicio,
                         to = fim,
                         by = bys[freq])
      }
    }
    
    next_period <- function(date, freq) {
      # obtem periodo seguinte com base em frequencia
      date <- as.Date(date)
      
      switch(freq,
             "D"   = date + 1,
             "M"   = lubridate::`%m+%`(date,months(1)),
             "Q"  = lubridate::`%m+%`(date,weeks(2)),
             "T" = lubridate::`%m+%`(date,months(3)),
             "S" = lubridate::`%m+%`(date,months(6)),
             "A"    = lubridate::`%m+%`(date,years(1)))
    }
    
    # Método para print e window -------------------------------------------
    
    print.macro_serie <- function(x, ...) {
      # print para objeto macro_serie
      
      attrs <- attributes(x)
      dados <- round(as.numeric(x), 2)
      attributes(dados) <- attrs
      
      if(inherits(dados, "ts")) {
        class(dados) <- "ts"
      }
      
      if(inherits(dados, "zoo")) {
        class(dados) <- "zoo"
      }
      
      print(dados)
    }
    
    janela <- function(x, inicio = NULL, fim = NULL, nonNA = TRUE) {
      # window para objeto macro_serie
    
        freq <- rec_frequencia(x)
      attrib <- attributes(x)
      inicio_serie <- rec_inicio(x)
      fim_serie <- rec_fim(x)
      
      freqs <- c(
        "D" = 365,
        "Q" = 24,
        "M" = 12,
        "T" = 4,
        "S" = 2,
        "A" = 1
      )
      
      if (freq %in% names(freqs)) {
        if(!is.null(inicio)) {
          inicio = as.Date(inicio)
          
          if(inicio > fim_serie) {
            return(NULL)
          }
            
          if(inicio >= inicio_serie) {
            inicio_serie <- inicio
          } 
          
          inicio_ano <- lubridate::year(inicio)
          if (freq == "Q") {
            if(!(lubridate::day(inicio) %in% c(1, 15))) {
              stop("Data de início inválida para frequência quinzenal")
            } 
            
            inicio_freq <- (lubridate::month(inicio) - 1)*2 + (2 - lubridate::day(inicio) %% 15)
          } else {
            inicio_freq <- (lubridate::month(inicio)-1)*12/freqs[freq] + 1
          }
          
          inicio_vec <- c(inicio_ano, inicio_freq)
        } else {
          if(nonNA) {
            inicio_vec <-  time(x)[which(!is.na(x))[1]]
            if(freq != "Q") {
              inicio_serie <- as.Date(paste(floor(inicio_vec), 
                                            round(((inicio_vec-floor(inicio_vec))*12)+1), 
                                            1, sep = "-"))
            } else {
              inicio_dia <- ifelse(round((inicio_vec-floor(inicio_vec))*24) %% 2 == 0, 1, 15)
              inicio_serie <- as.Date(paste(floor(inicio_vec), 
                                            round(((inicio_vec-floor(inicio_vec))*12)+1), 
                                            inicio_dia, sep = "-"))
            }
          } else {
            inicio_vec = NULL
          }
        }
          
        
        if(!is.null(fim)) {
          fim = as.Date(fim)
          
          if(fim < inicio_serie) {
            return(NULL)
          }
          
          if(fim <= fim_serie) {
            fim_serie <- fim
          }
          
          fim_ano <- lubridate::year(fim)
          if (freq == "Q") {
            if(!(lubridate::day(fim) %in% c(1, 15))) {
              stop("Data de fim inválida para frequência quinzenal")
            } 
            
            fim_freq <- (lubridate::month(fim) - 1)*2 + (2 - lubridate::day(fim) %% 15)
          } else {
            fim_freq <- (lubridate::month(fim)-1)*12/freqs[freq] + 1
          }
          fim_vec <- c(fim_ano, fim_freq)
        } else {
          fim_vec = NULL
        }
      } 
      
      serie <- x
      class(serie) <- setdiff(class(serie), "macro_serie")
      
      suppressWarnings({
        serie <- window(serie, 
                        start = inicio_vec,
                        end = fim_vec)
      })
      
      attr_serie <- attributes(serie)[setdiff(names(attributes(serie)), "class")]
      attrib[names(attr_serie)] <- attr_serie
      attributes(serie) <- attrib
      attr(serie, "inicio") <- as.character(inicio_serie)
      attr(serie, "fim") <- as.character(fim_serie)
      
      return(serie)
    }
    
    # Lista datas ----------------------------------
    
    lista_datas <- function(serie) {
      UseMethod("lista_datas")
    }
    
    lista_datas.default <- function(serie) {
      stop("Método válido somente para objeto macro_serie")
    }
    
    lista_datas.macro_serie <- function(serie) {
      freq <- rec_frequencia(serie)
      inicio <- rec_inicio(serie)
      fim <- rec_fim(serie)
      
      if(inherits(serie, "ts")) {
        data <- gera_datas(freq = freq,
                           inicio = inicio,
                           fim = fim)
      }
      
      if(inherits(serie, "zoo")) {
        data <- as.Date(index(serie))
      }
      
      return(data)
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
    
    # Obtem séries --------------------------------------------
    obtem_series <- function(series,
                             medidas = NULL,
                             frequencias = NULL,
                             territorios = NULL,
                             inicio = NULL,
                             mods = NULL,
                             sas = NULL,
                             cache = TRUE,
                             force = FALSE,
                             atual = TRUE,
                             ignora_nulo = TRUE) {
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
      
      if (!is.null(sas)) {
        if (length(sas) == 1) {
          sas <- rep(sas, length(series))  
        } else if (length(sas) != length(series)) {
          stop("'sa' deve ter tamanho 1 ou o mesmo de 'series'.")
        }
      } else {
        sas <- rep(FALSE, n)
      }
      
      if (is.null(mods)) {
        mods <- rep(list(NULL), n)
      }
      
      if(!is.null(inicio)) {
        inicios <- rep(inicio, n)
      } else {
        inicios <- rep(list(NULL), n)
      }
      
      caches <- rep(cache, n)
      forces <- rep(force, n)
      atuals <- rep(atual, n)
      
    ## Extrair série -----------------------------------------------------
    
    extrai_serie <- function(serie,
                             medida,
                             frequencia,
                             territorio,
                             sa,
                             mods,
                             inicio,
                             cache,
                             force,
                             atual) {
      # função para extrair uma única série
      
      if (!is.character(serie[1])) {
        stop("Série deve ser um vetor de caracteres")
      }
      
      if(force && !atual) {
        return(NULL)
      }
      
      ### Regra -----------------------------------------
      
      class_regra <- function(serie,
                              medida = NULL,
                              frequencia = NULL,
                              territorio = NULL) {
        # Cria regra de formação com base no nome da 'serie' e na lista pré definida de séries 
        
        serie = serie[1]
        
        serie_info <- gemac_series[[serie]]$info
        serie_padrao <- gemac_series[[serie]]$padrao
        
        if (is.null(medida)) {
          medida = serie_padrao[["medida"]]
        }
        
        if (is.null(frequencia)) {
          frequencia = serie_padrao[["frequencia"]]
        }
        
        if (is.null(territorio)) {
          if (is.null(serie_padrao[["territorio"]])) {
            classe <- gemac_series[[serie]][[medida]][[frequencia]]$metodo
            unidade <- gemac_series[[serie]][[medida]][[frequencia]]$unidade
            dados <- gemac_series[[serie]][[medida]][[frequencia]]$dados
            inicio <- gemac_series[[serie]][[medida]][[frequencia]]$inicio
            
            att_list <- list(
              class = classe,
              unidade = unidade,
              inicio = inicio,
              medida = medida,
              frequencia = frequencia
            )
            
            att_list <- c(serie_info, att_list)
            
            obj <- structure(.Data = dados, .Names = NULL)
            attributes(obj) <- c(attributes(obj), att_list)
            
            return(obj)
          }
          
          territorio = serie_padrao[["territorio"]]
        }
        
        classe <- gemac_series[[serie]][[medida]][[frequencia]][[territorio]]$metodo
        if (is.null(classe)) {
          return(NULL)
        }
        unidade <- gemac_series[[serie]][[medida]][[frequencia]][[territorio]]$unidade
        if (is.null(unidade)) {
          return(NULL)
        }
        dados <- gemac_series[[serie]][[medida]][[frequencia]][[territorio]]$dados
        if (is.null(dados)) {
          return(NULL)
        }
        inicio <- gemac_series[[serie]][[medida]][[frequencia]][[territorio]]$inicio
        if (is.null(inicio)) {
          return(NULL)
        }
        
        att_list <- list(
          serie = serie,
          class = classe,
          unidade = unidade,
          inicio = inicio,
          medida = medida,
          frequencia = frequencia,
          territorio = territorio
        )
        
        att_list <- c(serie_info, att_list)
        
        obj <- structure(.Data = dados)
        attributes(obj) <- c(attributes(obj), att_list)
        
        return(obj)
      }
      
      attr_regra <- function(ms, regra) {
        attr(ms, "tema") <- rec_tema(regra)
        attr(ms, "descricao") <- rec_descricao(regra)
        attr(ms, "serie") <- rec_serie(regra)
        attr(ms, "unidade") <- rec_unidade(regra)
        attr(ms, "medida") <- rec_medida(regra)
        attr(ms, "territorio") <- rec_territorio(regra)
        
        return(ms)
      }

      ### Repositório -----------------------------
      
      extrai_repo <- function(regra, sa = FALSE) {
        #extrai série contida no repositorio local
        
        serie <- rec_serie(regra)
        medida <- rec_medida(regra)
        frequencia <- rec_frequencia(regra)
        territorio <- rec_territorio(regra)
        tema <- rec_tema(regra)
        descricao <- rec_descricao(regra)
        
        if(is.null(getOption("macroseries.data_path"))) {
          stop("Defina o endereço do repo com 'options(macroseries.data_path = J:/meu/repo'")
        }
        
        repo_wd <- getOption("macroseries.data_path")
        
        suf_medidas <- c(
          "Variação mensal"= "varm",
          "Peso" = "peso",
          "Contribuição" = "contrib",
          "Inflação relativa" = "inflarel"
        )
        
        suf_territorios <- c(
          "br", "belem", "fortal", "recif", "salva", "beloh",
          "vitor", "riode", "saopa", "curit", "porto", "riobr",
          "saolu", "araca", "campo", "goian", "brasi"
        )
        
        names(suf_territorios) <- c(
          "Brasil", "Belém - PA", "Fortaleza - CE", "Recife - PE", "Salvador - BA", "Belo Horizonte - MG",
          "Grande Vitória - ES", "Rio de Janeiro - RJ", "São Paulo - SP", "Curitiba - PR", "Porto Alegre - RS", "Rio Branco - AC",
          "São Luís - MA", "Aracaju - SE", "Campo Grande - MS", "Goiânia - GO", "Brasília - DF"
        )
     
        serie_repo <- paste(serie, suf_medidas[medida], frequencia, suf_territorios[territorio], sep = "_")
        if (sa) serie_repo <- paste0(serie_repo, "_sa")
        serie_repo <- paste0("repo/", tema, "/", serie_repo, ".rds")
        
        serie_repo <- paste0(repo_wd, "/", serie_repo)
        
        if(!file.exists(serie_repo)) {
          message(cat("Série ", descricao, medida, " inexistente no repositório"))
          
          return(NULL)
        }
        
        ms <- readRDS(file = serie_repo)
          
        return(ms)
      }
      
      salva_repo <- function(ms, regra, sa = FALSE) {
        #salva serie no repositorio
        
        serie <- rec_serie(regra)
        medida <- rec_medida(regra)
        frequencia <- rec_frequencia(regra)
        territorio <- rec_territorio(regra)
        tema <- rec_tema(regra)
        descricao <- rec_descricao(regra)
        
        if(is.null(getOption("macroseries.data_path"))) {
          stop("Defina o endereço do repo com 'options(macroseries.data_path = J:/meu/repo'")
        }
        
        repo_wd <- getOption("macroseries.data_path")
        
        suf_medidas <- c(
          "Variação mensal"= "varm",
          "Peso" = "peso",
          "Contribuição" = "contrib",
          "Inflação relativa" = "inflarel"
        )
        
        suf_territorios <- c(
          "br", "belem", "fortal", "recif", "salva", "beloh",
          "vitor", "riode", "saopa", "curit", "porto", "riobr",
          "saolu", "araca", "campo", "goian", "brasi"
        )
        
        names(suf_territorios) <- c(
          "Brasil", "Belém - PA", "Fortaleza - CE", "Recife - PE", "Salvador - BA", "Belo Horizonte - MG",
          "Grande Vitória - ES", "Rio de Janeiro - RJ", "São Paulo - SP", "Curitiba - PR", "Porto Alegre - RS", "Rio Branco - AC",
          "São Luís - MA", "Aracaju - SE", "Campo Grande - MS", "Goiânia - GO", "Brasília - DF"
        )
        
        
        serie_repo <- paste(serie, suf_medidas[medida], frequencia, suf_territorios[territorio], sep = "_")
        if (sa) serie_repo <- paste0(serie_repo, "_sa")
        serie_repo <- paste0("repo/", tema, "/", serie_repo, ".rds")
        
        serie_repo <- paste0(repo_wd, "/", serie_repo)
        
        saveRDS(ms, file = serie_repo)
      }
          
      ### Atualização ------------------
      
      atual_serie <- function(regra, ...) {
        # Atualiza série com base na regra de formação dependendo do método (class)
        UseMethod("atual_serie")
      }
      
      atual_serie.default <- function(regra, ...) {
        stop("Método inválido para esse objeto.")
      }
      
      ##### Métodos primários --------------------------------------------
      
      # Métodos de consulta aos dados originais da WEB
      
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
      
      ###### SNIPC --------------------------------------------
      
      snipc <- function(tabela,
                        codigo,
                        variavel,
                        territorio,
                        cache) {
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
        
        if(exists(table_name, env = .cache_env) && cache) {
          return(get(table_name, envir = .cache_env))
        }
        
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
        
        assign(table_name, dados, envir = .cache_env)
        
        return(dados)
      }
      
      atual_serie.snipc_puro <- function(regra, cache, ...) {
        # Método para extração de série do SNIPC
        
        cod <- regra$codigo
        tab <- regra$tabela
        terr <- rec_territorio(regra)
        freq <- rec_frequencia(regra)
        medi <- rec_medida(regra)
        
        dados <- snipc(tabela = tab,
                    codigo = cod,
                    variavel = medi,
                    territorio = terr,
                    cache = cache)
        
        if (is.null(dados)) {
          return(NULL)
        }
        
        ms <- ms_dados(dados = dados,
                          regra = regra)
        
        return(ms)    
      }
      
      ###### SGS --------------------------------------------
      
      sgs <- function(codigo, 
                      freq, 
                      cache,
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
        if (exists(series_name, env = .cache_env) && cache) {
          
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
      
      # cria série do SGS com base na regra
      atual_serie.sgs_puro <- function(regra, cache, periodo_inicial) {
        codigo <- regra$codigo
        freq <- rec_frequencia(regra)
        
        inicio_diario = NULL
        if(freq == "D") {
          inicio_diario <- periodo_inicial
        }
        
        dados <- sgs(
          codigo = codigo,
          freq = freq,
          cache = cache,
          inicio_diario = inicio_diario
        )
        
        if(is.null(dados)) {
          return(NULL)
        }
        
        ms <- ms_dados(dados = dados,
                          regra = regra)
        
        return(ms)    
      }
      
      #### Métodos recursivos ---------------------------------------
      
      # Métodos que combinam séries entre si
      
      ##### Séries combinadas ----------------------------------------------------------
      
      atual_serie.series_resume <- function(regra, cache, periodo_inicial) {
        # Resume uma coleção de séries com base numa função
        
        freq <- rec_frequencia(regra)
        inicio <- rec_inicio(regra)
       
        periodo_inicial <- as.Date(max(periodo_inicial, inicio))
        
        series <- regra$series
        medidas <- regra$medidas
        frequencias <- regra$frequencias
        territorios <- regra$territorios
        sas <- regra$sas
        func <- regra$func
        
        ms <- obtem_series(series = series,
                           medidas = medidas,
                           frequencias = frequencias,
                           territorios = territorios,
                           inicio = periodo_inicial,
                           cache = cache,
                           atual = atual)
        
        if(is.null(ms)) {
          return(NULL)
        }
        
        ms <- resume_serie(ms, fun = func)
        ms <- janela(ms)
  
        ms <- attr_regra(ms, regra)    
        
        return(ms)
      }
      
      atual_serie.series_aplica <- function(regra, cache, periodo_inicial) {
        # Aplica uma função em uma série
        
        terr <- rec_territorio(regra)
        freq <- rec_frequencia(regra)
        inicio <- rec_inicio(regra)
        
        periodo_inicial <- as.Date(max(periodo_inicial, inicio))
        
        series <- regra$series
        medidas <- regra$medidas
        frequencias <- regra$frequencias
        territorios <- regra$territorios
        sas <- regra$sas
        func <- regra$func
        larg <- regra$larg
        
        ms <- obtem_series(series = series,
                            medidas = medidas,
                            frequencias = frequencias,
                            territorios = territorios,
                            inicio = periodo_inicial,
                            cache = cache,
                           atual = atual)
        
        if(is.null(ms)) {
          return(NULL)
        }
        
        ms <- aplica_serie(ms, fun = func, width = larg)
        ms <- janela(ms)
        
        return(ms)
      }
      
      ##### IPCA e IPCA-15 ---------------------------------------------------------------
      
      atual_serie.ipca_composicao <- function(regra, cache, ...) {
        # Método para cálculo de IPCA e IPCA-15 com base na composição de outras séries calculadas
        
        series <- regra$series
        exclusao <- regra$exclusao
        
        medi <- rec_medida(regra)
        terr <- rec_territorio(regra)
        freq <- rec_frequencia(regra)
        
        ms_pesos <- obtem_series(series = series,
                                  medidas = "Peso",
                                  frequencias = freq,
                                  territorios = terr,
                                  inicio = NULL,
                                  cache = cache,
                                 atual = atual)
        
        if(is.null(ms_pesos)) {
          return(NULL)
        }
        
        ms_contribs <- obtem_series(series = series,
                                     medidas = "Contribuição",
                                     frequencias = freq,
                                     territorios = terr,
                                    inicio = NULL,
                                    cache = cache,
                                    atual = atual)
        
        if(is.null(ms_contribs)) {
          return(NULL)
        }
        
        if(!is.null(exclusao)) {
          ms_peso_cheia <- obtem_series(series = exclusao,
                                    medidas = "Peso",
                                    frequencias = freq,
                                    territorios = terr,
                                    inicio = NULL,
                                    cache = cache,
                                    atual = atual)
          
          if(is.null(ms_peso_cheia)) {
            return(NULL)
          }
          
          ms_contrib_cheia <- obtem_series(series = exclusao,
                                       medidas = "Contribuição",
                                       frequencias = freq,
                                       territorios = terr,
                                       inicio = NULL,
                                       cache = cache,
                                       atual = atual)
          
          if(is.null(ms_contrib_cheia)) {
            return(NULL)
          }
        }
        
        contrib <- resume_serie(ms_contribs, fun = "sum(x)")
        peso <- resume_serie(ms_pesos, fun = "sum(x)")
        
        if(!is.null(exclusao)) {
          contrib <- combina_serie(ms_contrib_cheia, contrib)
          contrib <- resume_serie(contrib, fun = "x[1]-x[2]")
          
          peso <- combina_serie(ms_peso_cheia, peso)
          peso <- resume_serie(peso, fun = "x[1]-x[2]")
        }
        
        if(medi == "Peso") {
          peso <- attr_regra(peso, regra)
          
          return(peso)
        }
        
        ms <- combina_serie(contrib, peso)
        ms <- resume_serie(ms, fun = "x[1]/x[2]*100")
        ms <- janela(ms)
        ms <- attr_regra(ms, regra)
        
        return(ms)
      }
      
      atual_serie.ipca_nucleo <- function(regra, cache, ...) {
        # Método para calcular os núcleos do IPCA e IPCA-15
        
        nucleo <- regra$nucleo
        series <- regra$series
        suav <- regra$suav
        serie_cheia <- regra$serie_cheia
        
        medi <- rec_medida(regra)
        terr <- rec_territorio(regra)
        freq <- rec_frequencia(regra)
        
        # extrai séries de variação mensal e pesos
        ms_pesos <- obtem_series(series = series,
                                  medidas = "Peso",
                                  frequencias = freq,
                                  territorios = terr,
                                 inicio = NULL,
                                 cache = cache,
                                 atual = atual)
        
        if(is.null(ms_pesos)) {
          return(NULL)
        }
        
        if (nucleo != "ms") { 
          ms_cheia <- obtem_series(series = serie_cheia,
                                    medidas = "Variação mensal",
                                    frequencias = freq,
                                    territorios = terr,
                                   inicio = NULL,
                                   cache = cache,
                                   atual = atual)
          
          if(is.null(ms_cheia)) {
            return(NULL)
          }
        }
        
        if(!is.null(suav)) {
          series_totais <- series
          series <- setdiff(series, suav)
        }
        
        ms_vars <- obtem_series(series = series,
                                 medidas = "Variação mensal",
                                 frequencias = freq,
                                 territorios = terr,
                                inicio = NULL,
                                cache = cache,
                                atual = atual)
        
        if(is.null(ms_vars)) {
          return(NULL)
        }
        
        if(!is.null(suav)) {
          ms_suav <- obtem_series(series = suav,
                                   medidas = "Variação mensal",
                                   frequencias = freq,
                                   territorios = terr,
                                  inicio = NULL,
                                  cache = cache,
                                  atual = atual)
          
          if(is.null(ms_suav)) {
            return(NULL)
          }
          
          ms_suav <- aplica_serie(ms_suav, 
                                  fun = function(x) (exp(mean(log(1+ x/100)))-1)*100,
                                  width = 12, 
                                  partial = TRUE)
          
          ms_vars <- combina_serie(ms_vars, ms_suav, fill = FALSE)
          ms_vars <- resume_serie(ms_vars, 
                                  fun = "vec <- numeric(length = ncol(ms_vars))\n
                                  vec[!(series_totais %in% suav)] <- x[1:length(series)]\n
                                  vec[series_totais %in% suav] <- x[(length(series)+1):length(series_totais)]\n
                                  return(vec)",
                                  series = series,
                                  suav = suav,
                                  series_totais = series_totais)
        }
        
        tsp_vars <- tsp(ms_vars)
        inicio_vars <- rec_inicio(ms_vars)
        fim_vars <- rec_fim(ms_vars)
        
        if(!is.null(suav)) {
          series <- series_totais
        }
        
        ms_peso_cheia <- obtem_series(series = serie_cheia,
                                       medidas = "Peso",
                                       frequencias = freq,
                                       territorios = terr,
                                      inicio = NULL,
                                      cache = cache,
                                      atual = atual)
        
        if(is.null(ms_peso_cheia)) {
          return(NULL)
        }
        
        comb_pesos <- combina_serie(x1 = ms_peso_cheia,
                                    x2 = ms_pesos)
        ms_pesos <- resume_serie(x = comb_pesos,
                                 fun = "x[-1]/x[1]*100")
        
        if (nucleo == "ms") {
          order_vars <- resume_serie(ms_vars, 
                                     fun = "order(x)")
          
          ms_pesos_ordem <- resume_serie(combina_serie(x1 = ms_pesos,
                                                       x2 = order_vars),
                                         fun = "order <- x[(length(x)/2+1):length(x)]\n
                                         pesos <- x[1:(length(x)/2)]\n
                                         return(pesos[order])")
          ms_vars_ordem <- resume_serie(combina_serie(x1 = ms_vars,
                                                      x2 = order_vars),
                                        fun = "order <- x[(length(x)/2+1):length(x)]\n
                                         vars <- x[1:(length(x)/2)]\n
                                         return(vars[order])")
          ms_pesos_acum <- resume_serie(ms_pesos_ordem, 
                                        fun = "cumsum(x)")
          ms_posicoes <- resume_serie(x = ms_pesos_acum, 
                                      fun = "c(which(x-20 > 0)[1], 
                                    x[which(x-20 > 0)[1]]-20, 
                                    which(x-80 > 0)[1], 
                                    80-x[which(x-80 > 0)[1]-1])")
          n = length(series)
          ms_combs <- combina_serie(x1 = combina_serie(x1 = ms_pesos_ordem, 
                                                       x2 = ms_vars_ordem), 
                                    x2 = ms_posicoes)
          
          ms <- resume_serie(ms_combs,
                             fun = "pesos <- x[1:n]\n
                                        vars <- x[(n+1):(2*n)]\n
                                        posi <- x[(2*n+1):length(x)]\n
                                        subset_p <- pesos[posi[1]:posi[3]]\n
                                        subset_p[1] <- posi[2]\n 
                                        subset_p[length(subset_p)] <- posi[4]\n
                                        subset_v <- vars[posi[1]:posi[3]]\n
                                        value <- sum(subset_p*subset_v)/sum(subset_p)\n
                                        return(value)",
                             n = n)
          ms <- attr_regra(ms, regra)
          ms <- janela(ms)
          
          return(ms)
        }
        
        if (nucleo == "dp") {
          ms_total <- combina_serie(ms_cheia, ms_vars)
          dif_vars <- resume_serie(ms_total, 
                                   fun = "x[-1]-x[1]")
          ms_sds <- aplica_serie(dif_vars, fun = sd, width = 48, partial = TRUE)
          comb_pesos <- combina_serie(x1 = ms_pesos,
                                      x2 = ms_sds)
          ms_pesos_ajus <- resume_serie(x = comb_pesos,
                                        fun = "x[1:ncols]/x[(ncols+1):(2*ncols)]",
                                        ncols = ncol(ms_sds))
          comb_series <- combina_serie(x1 = ms_vars,
                                       x2 = ms_pesos_ajus)
          ms <- resume_serie(x = comb_series,
                             fun = "sum(x[1:ncols]*x[(ncols+1):(2*ncols)])/sum(x[(ncols+1):(2*ncols)])",
                             ncols = ncol(ms_vars))
          ms <- attr_regra(ms, regra)
          ms <- janela(ms)
          
          return(ms) 
        }
        
        if (nucleo == "p55") {
          order_vars <- resume_serie(ms_vars, 
                                     fun = "order(x)")
          comb_series <- combina_serie(x1 = combina_serie(x1 = ms_pesos,
                                       x2 = order_vars),
                                       x2 = ms_vars)
          n = length(series)
          ms <- resume_serie(x = comb_series, 
                             fun = "x1 <- x[1:n]\n
                                         x2 <- x[(n+1):(2*n)]\n
                                         x3 <- x[(2*n+1):(3*n)]\n
                                         posi <- which(cumsum(x1[x2])-55 > 0)[1]\n
                                         return(sort(x3)[posi])",
                             n = n)
          ms <- attr_regra(ms, regra)
          ms <- janela(ms)
          
          return(ms) 
        }
      }

      ### Ajuste sazonal ---------------------------------------------------------
      
      ajuste_saz <- function(ms, ...) {
        UseMethod("ajuste_saz")
      }
      
      ajuste_saz.default <- function(ms, ...) {
        stop("Método definido para macro_serie")
      }
      
      ajuste_saz.macro_serie <- function(ms, ...) {
        freq <- rec_frequencia(ms)
        unid <- rec_unidade(ms)
        
        if (freq %in% c("D", "Q", "S", "A")) {
          message("Frequencia incompatível com ajuste sazonal.")
          return(ms)
        }
        
        if (unid == "%") {
          ms <- janela(ms, inicio = "1999-01-01")
        }
        
        trans_fun <- if (unid %in% c("Índice", "R$", "US$")) "log" else "none"
        
        regressores <- c()
        if (freq == "M") regressores <- c("td", "easter[8]") # incluir 'carnival' depois
        if (freq == "Q") regressores <- c("easter[8]")
        
        tryCatch(expr = {
          sa <- seasonal::seas(ms,
                               transform.function = trans_fun,
                               automdl = "",
                               outlier.types = c("ao", "ls", "tc"),
                               regression.variables = regressores,
                               seats = list())
        }, error = function(e) {
          return(ms)
        })
        
        # diag <- summary(sa)$diagnostics
        # if (diag$M7 > 1.0 || diag$Qstat.pvalue < 0.05) {
        #   message("Diagnóstico indica picos sazonais no espectro de resíduos.")
        # }
        
        ms_final <- seasonal::final(sa)
        attributes(ms_final) <- attributes(ms)
        
        return(ms_final)
      }
      

      ## Média móvel de n períodos ----------------------------------------------------------------

      media_movel_n <- function(ms, n) {
        attrs <- attributes(ms)
        unid <- attrs$unidade
        x <- as.numeric(ms)
        
        if(unid == "%") {
          f = function(x) {
            val = (exp(mean(log(1+x/100)))-1)*100
            return(val)
          }
        } else {
          f = mean
        }
        
        x <- zoo::rollapply(x, width = n, FUN = f, 
                       fill = NA, align = "right")
        attributes(x) <- attrs
        
        return(x)
      }
      
      ## Acumulado de n períodos ----------------------------------------------------------------
      
      acumulado_n <- function(ms, n) {
        attrs <- attributes(ms)
        unid <- attrs$unidade
        x <- as.numeric(ms)
        
        if(unid == "%") {
          f = function(x) {
            val = (exp(sum(log(1+x/100)))-1)*100
            return(val)
          }
        } else {
          f = sum
        }
        
        x <- zoo::rollapply(x, width = n, FUN = f, 
                            fill = NA, align = "right")
        attributes(x) <- attrs
        
        return(x)
      }
      
      ## Acumulado total ---------------------------------------------------------

      acumulado <- function(ms) {
        attrs <- attributes(ms)
        unid <- attrs$unidade
        x <- as.numeric(ms)
        
        if(unid == "%") {
          f = function(x) {
            val = (exp(cumsum(log(1+x/100)))-1)*100
            return(val)
          }
        } else {
          f = cumsum
        }
        
        x <- f(x)
        attributes(x) <- attrs
        
        return(x)
      }
      
      ## Anualizado --------------------------------------------------------------

      anualizado <- function(ms) {
        attrs <- attributes(ms)
        unid <- attrs$unidade
        freq <- attrs$frequencia
        
        if(freq == "D") {
          message("Série diária não pode ser acumulada.")
          return(ms)
        }
        
        freqs <- c(
          "Q" = 12,
          "M" = 12,
          "T" = 4,
          "S" = 2,
          "A" = 1
        )
        
        x <- as.numeric(ms)
        
        if(unid == "%") {
          x = ((1+x/100)^freqs[freq]-1)*100
        } else {
          x = freqs[freq]*x
        }
        
        attributes(x) <- attrs
        
        return(x)
      }
      
      lista_funs <- list("12 meses" = function(x) anualizado(media_movel_n(x, n = 12)), 
                         "Acumulado" = acumulado,
                         "MM3m" = function(x) media_movel_n(x, n = 3),
                         "MM3ma" = function(x) anualizado(media_movel_n(x, n = 3)),
                         "Anualizado"  = anualizado)
      
      ## Encontra regra e extrai --------------------------------------------------
  
      regra <- class_regra(
        serie,
        medida = medida,
        frequencia = frequencia,
        territorio = territorio
      )
      
      inicio_regra <- rec_inicio(regra)
      
      if(is.null(regra)) {
        message("Sem regra de formação para a combinação escolhida.")
        return(NULL)
      }
      
      if(!is.null(mods)) {
        aplica_funs <- function(x) {
          for (nm in mods) {
            f <- lista_funs[[nm]]
            x <- f(x) 
          }
          x
        }
      } else {
        aplica_funs <- function(x) x
      }
      
      if(sa && !atual) {
        ms <- extrai_repo(regra, sa = FALSE)
        ms <- ajuste_saz(ms)
        ms <- aplica_funs(ms)
        
        return(ms)
      } 
      
      if(sa && atual) {
        ms <- extrai_serie(serie = serie,
                           medida = medida,
                           frequencia = frequencia,
                           territorio = territorio,
                           sa = FALSE,
                           mods = NULL,
                           inicio = inicio,
                           cache = cache,
                           force = force,
                           atual = atual)
        ms <- ajuste_saz(ms)
        salva_repo(ms, regra, sa = TRUE)
        ms <- aplica_funs(ms)
        
        return(ms)
      }
      
      if(!force && !sa) {
        ms <- extrai_repo(regra)
      }
      
      if(atual) {
        if (!exists("ms", envir = environment(), inherits = FALSE) || is.null(ms)) {
          ms <- atual_serie(regra = regra, 
                            cache = cache, 
                            periodo_inicial = inicio_regra
          )
          
          if (is.null(ms)) {
            message(paste("Não foi encontrada atualização da série", rec_serie(regra)))
            return(NULL)
          }
          
          salva_repo(ms, regra)
          ms <- aplica_funs(ms)
          
          return(ms)
        } else {
          if(is.null(inicio)) {
            inicio <- inicio_regra
          }
          
          ms_atual <- atual_serie(
            regra = regra,
            cache = cache,
            periodo_inicial = inicio
          )
          
          if(!is.null(ms_atual)) {
            if(rec_fim(ms_atual) > rec_fim(ms)) {
              ms <- combina_serie(ms, ms_atual, fill = FALSE)
              ms <- resume_serie(ms, fun = "aglut(x)")
            }
          }
          
          salva_repo(ms, regra)
          ms <- aplica_funs(ms)
          
          return(ms)
        }
      }
      
      salva_repo(ms, regra)
      ms <- aplica_funs(ms)
      
      return(ms)
    }
  
    ## Obtem lista de séries ---------------------------------------
      
      ms_series <- mapply(
        function(serie,
                 medida,
                 frequencia,
                 territorio,
                 sa,
                 mods,
                 inicio,
                 cache,
                 force,
                 atual) {
          extrai_serie(serie,
                       medida,
                       frequencia,
                       territorio,
                       sa,
                       mods,
                       inicio,
                       cache,
                       force,
                       atual)
        },
        series,
        medidas,
        frequencias,
        territorios,
        sas,
        mods,
        inicios,
        caches,
        forces,
        atuals,
        SIMPLIFY = FALSE
      )
      
    if(!ignora_nulo && sum(sapply(ms_series, is.null)) > 0) {
        return(NULL)
    }
    
    if(sum(sapply(ms_series, is.null)) == length(ms_series)) {
      return(NULL)
    }
    
    ms_series <- Filter(function(x) !is.null(x), ms_series)
    
    ms <- Reduce(function(x, y) combina_serie(x, y, fill = FALSE), ms_series) 
  
    return(ms)
  }
  
    # Realiza a extração e retorna --------------------------------------------

  ms_series <- obtem_series(series = args[["serie"]],
                            medidas = args[["medida"]],
                            frequencias = args[["frequencia"]],
                            territorios = args[["territorio"]],
                            sas = args[["sa"]],
                            mods = args[["mods"]],
                            inicio = inicio,
                            cache = cache,
                            force = force,
                            atual = atual,
                            ignora_nulo = ignora_nulo)  
    
  return(ms_series)
}
