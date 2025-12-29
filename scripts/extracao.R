extrai_serie <- function(repo, inicio = NULL, 
                         atual = getOption("macroseries.atualiza", FALSE)) {
  # tenta extrair do repo
  regra <- extrai_regra(repo)
  ms <- extrai_repo(repo, regra)
  
  # se atualizar é permitido e/ou ms ausente -> buscar atualização via S3 method
  if (atual) {
    periodo_inicial <- if (is.null(inicio)) as.Date(regra$inicio) else as.Date(inicio)
    ms_atual <- atual_serie(regra, periodo_inicial = periodo_inicial)
    if (!is.null(ms_atual)) {
      # se havia ms local e ms_atual é mais recente, combina; senão salva
      if (is.null(ms)) {
        ms <- ms_atual
        salva_repo(ms, repo, regra)
      } else {
        # combinar apenas se ms_atual tem novas observações
        if (as.Date(ms_atual$meta$fim) > as.Date(ms$meta$fim)) {
          ms <- combina_serie(ms, ms_atual, fill = FALSE)
          ms <- resume_serie(ms, fun = aglut)
          ms$meta <- regra
          salva_repo(ms, repo, regra)
        }
      }
    }
  }
  
  ms
}

# wrapper para extracao safe
extrai_safe <- function(repo, inicio, 
                        atual = getOption("macroseries.atualiza", FALSE), 
                        ignora_nulo = TRUE) {
  tryCatch({
    ms <- extrai_serie(repo, inicio = inicio, atual = atual)
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
