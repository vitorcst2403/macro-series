# Repositório -----------------------------

# Lookup de medidas com validação
sufixos_medi <- function(medida) {
  suf_medidas <- c(
    "Variação mensal" = "varm",
    "Peso" = "peso",
    "Contribuição" = "contrib",
    "Inflação relativa" = "inflarel"
  )
  
  if (!medida %in% names(suf_medidas)) {
    stop(sprintf("Medida desconhecida: '%s'. Medidas válidas: %s",
                 medida, paste(names(suf_medidas), collapse = ", ")),
         call. = FALSE)
  }
  
  return(unname(suf_medidas[medida]))
}

# Lookup de territórios com validação
sufixos_terri <- function(territorio) {
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
  
  if (!territorio %in% names(suf_territorios)) {
    stop(sprintf("Território desconhecido: '%s'. Territórios válidos: %s",
                 territorio, paste(names(suf_territorios), collapse = ", ")),
         call. = FALSE)
  }
  
  return(unname(suf_territorios[territorio]))
}

extrai_repo <- function(regra, sa = FALSE) {
  # extrai série contida no repositorio local
  
  serie <- rec_serie(regra)
  medida <- rec_medida(regra)
  frequencia <- rec_frequencia(regra)
  territorio <- rec_territorio(regra)
  tema <- rec_tema(regra)
  descricao <- rec_descricao(regra)
  
  data_path <- getOption("macroseries.data_path")
  if (is.null(data_path)) {
    stop("Defina o endereço do repo com options(macroseries.data_path = 'J:/meu/repo')")
  }
  
  repo_wd <- data_path
  
  serie_repo <- paste(serie, sufixos_medi(medida), frequencia, sufixos_terri(territorio), sep = "_")
  if (sa) serie_repo <- paste0(serie_repo, "_sa")
  caminho_serie <- file.path(repo_wd, "repo", tema)
  if (!dir.exists(caminho_serie)) {
    dir.create(caminho_serie, recursive = TRUE, showWarnings = FALSE)
  }
  serie_repo <- file.path(caminho_serie, paste0(serie_repo, ".rds"))
  
  # usar caminho absoluto para consistência
  serie_repo_abs <- normalizePath(serie_repo, winslash = "/", mustWork = FALSE)
  if (!file.exists(serie_repo_abs)) {
    message(sprintf("Série '%s' (%s) inexistente no repositório", descricao, medida))
    return(NULL)
  }
  
  key <- cache_key_from_path(serie_repo_abs)
  # tenta obter do cache (só retorna se mtime idêntico)
  ms <- cache_get_file(key, serie_repo_abs)
  if (!is.null(ms)) {
    return(ms)
  }
  
  # caso contrário, carrega do disco e armazena no cache
  ms <- readRDS(file = serie_repo_abs)
  cache_set_file(key, ms, serie_repo_abs)
  return(ms)
}

salva_repo <- function(ms, regra, sa = FALSE) {
  # salva serie no repositorio
  
  serie <- rec_serie(regra)
  medida <- rec_medida(regra)
  frequencia <- rec_frequencia(regra)
  territorio <- rec_territorio(regra)
  tema <- rec_tema(regra)
  descricao <- rec_descricao(regra)
  
  data_path <- getOption("macroseries.data_path")
  if (is.null(data_path)) {
    stop("Defina o endereço do repo com options(macroseries.data_path = 'J:/meu/repo')")
  }
  
  repo_wd <- data_path
  
  serie_repo <- paste(serie, sufixos_medi(medida), frequencia, sufixos_terri(territorio), sep = "_")
  if (sa) serie_repo <- paste0(serie_repo, "_sa")
  serie_repo <- file.path("repo", tema, paste0(serie_repo, ".rds"))
  serie_repo <- file.path(repo_wd, serie_repo)
  
  # garante diretório
  dir.create(dirname(serie_repo), recursive = TRUE, showWarnings = FALSE)
  
  # salva atomically (escreve em tmp e renomeia)
  tmpfile <- tempfile(pattern = "tmp_series_", tmpdir = dirname(serie_repo))
  saveRDS(ms, file = tmpfile)
  # file.rename retorna TRUE se bem sucedido
  success <- file.rename(tmpfile, serie_repo)
  
  # invalidar cache (caso exista) para garantir consistência
  key <- cache_key_from_path(normalizePath(serie_repo, winslash = "/", mustWork = FALSE))
  cache_remove(key)
  # opcionalmente, recachear imediatamente:
  cache_set_file(key, ms, serie_repo)
  
  if (!success) {
    # tentativa final: salvar diretamente (menos seguro)
    warning("salva_repo: não foi possível renomear arquivo temporário; salvando diretamente.")
    saveRDS(ms, file = serie_repo)
  }
}