extrai_serie <- function(regra, inicio = NULL, 
                         atual = getOption("macroseries.atualiza", FALSE), .local_cache) {
  if (is.null(.local_cache)) .local_cache <- make_local_cache()
  
  key <- regra_key(regra)
  
  # já computado?
  if (!is.null(.local_cache$results[[key]])) return(.local_cache$results[[key]])
  
  # tenta extrair do repo (usa cache de arquivo implementado)
  ms <- extrai_repo(regra)
  if (is.null(ms) && !atual) {
    .local_cache$results[[key]] <- NULL
    return(NULL)
  }
  
  # se atualizar é permitido e/ou ms ausente -> buscar atualização via S3 method
  if (atual) {
    periodo_inicial <- if (is.null(inicio)) rec_inicio(regra) else as.Date(inicio)
    ms_atual <- atual_serie(regra, periodo_inicial = periodo_inicial, .local_cache = .local_cache)
    if (!is.null(ms_atual)) {
      # se havia ms local e ms_atual é mais recente, combina; senão salva
      if (is.null(ms)) {
        ms <- ms_atual
        salva_repo(ms, regra)
      } else {
        # combinar apenas se ms_atual tem novas observações
        if (rec_fim(ms_atual) > rec_fim(ms)) {
          ms <- combina_serie(ms, ms_atual, fill = FALSE)
          ms <- resume_serie(ms, fun = aglut)
          ms <- attr_regra(ms, regra)
          salva_repo(ms, regra)
        }
      }
    }
  }
  
  # salvar em cache local e limpar visited
  .local_cache$results[[key]] <- ms
  ms
}

# wrapper para extracao safe
extrai_safe <- function(serie, medida, frequencia, territorio, inicio, 
                        atual = getOption("macroseries.atualiza", FALSE), ignora_nulo = TRUE, 
                        .local_cache = NULL) {
  if (is.null(.local_cache)) .local_cache <- make_local_cache()
  tryCatch({
    regra <- class_regra(serie, medida = medida, frequencia = frequencia, territorio = territorio)
    if (is.null(regra)) return(NULL)
    ms <- extrai_serie(regra, inicio = inicio, atual = atual, .local_cache = .local_cache)
    ms
  }, error = function(e) {
    warning(sprintf("Erro ao extrair '%s': %s", serie, e$message))
    NULL
  })
}

# altera opção de atualização
opcao_atualiza <- function(opcao = FALSE) {
  if(!is.logical(opcao)) {
    stop("opcao deve ser lógico.")
  }
  
  options(macroseries.atualiza = opcao)
}
