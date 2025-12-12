consulta <- function(nome,
                     ...,
                     inicio = NULL,
                     fim = NULL,
                     atualiza = FALSE,
                     ignora_nulo = TRUE) {
  # função de consulta às macro series
  gemac_series <- get_gemac_series()
  codigos_7060 <- get_codigos_7060()
  names_tbl <- get_names_tbl()
  
  opcao_original <- getOption("macroseries.atualiza", FALSE)
  
  if (atualiza) {
    options(macroseries.atualiza = TRUE)
    on.exit(options(macroseries.atualiza = opcao_original), add = TRUE)
  }
  
  nomes <- c(...)
  nomes <- c(nome, nomes)
  
  split_first <- function(s) {
    pos <- regexpr("&", s)
    if (pos == -1) return(s)
    c(substr(s, 1, pos - 1), substr(s, pos + 1, nchar(s)))
  }
  
  nome_mod <- lapply(nomes, function(x) trimws(split_first(x)))
  nomes <- sapply(nome_mod, function(x)
    x[1])
  mods <- sapply(nome_mod, function(x) {
    x <- x[-1]
    if (length(x) == 0) {
      return(NA)
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
  
  args <- encontra_series(nomes)
  sas <- args[["sa"]]
  names(sas) <- args[["serie"]]
  names(mods) <- args[["serie"]]
  
  # Realiza a extração e retorna --------------------------------------------
  
  ms_series <- coleta_series(
    series = args[["serie"]],
    medidas = args[["medida"]],
    frequencias = args[["frequencia"]],
    territorios = args[["territorio"]],
    inicio = inicio,
    ignora_nulo = ignora_nulo,
    combina = FALSE
  )
  
  series = names(ms_series)
  
  ms_series <- lapply(series, function(x) sa_series(ms_series[[x]], sas[x]))
  names(ms_series) <- series
  
  ms_series <- lapply(series, function(x) modifica_series(ms_series[[x]], mods[[x]]))
  names(ms_series) <- series
  
  ms <- Reduce(function(x, y)
    combina_serie(x, y, fill = FALSE), ms_series)
  
  ms <- janela(ms, inicio = inicio, fim = fim)
  
  return(ms)
}
