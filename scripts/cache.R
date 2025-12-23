# scripts/cache.R

# Ambiente de cache em memória (in-memory cache)
.cache_env <- new.env(parent = emptyenv())

# --- Helpers genéricos de cache (em memória) ------------------------------
cache_set <- function(key, value) {
  assign(key, list(value = value, time = Sys.time()), envir = .cache_env)
  invisible(TRUE)
}

cache_get <- function(key) {
  if (!exists(key, envir = .cache_env, inherits = FALSE)) return(NULL)
  get(key, envir = .cache_env, inherits = FALSE)$value
}

cache_has <- function(key) {
  exists(key, envir = .cache_env, inherits = FALSE)
}

cache_remove <- function(key) {
  if (exists(key, envir = .cache_env, inherits = FALSE)) rm(list = key, envir = .cache_env)
  invisible(TRUE)
}

cache_info <- function() {
  keys <- ls(envir = .cache_env, all.names = TRUE)
  info <- lapply(keys, function(k) {
    item <- get(k, envir = .cache_env)
    list(key = k, time = item$time, size = object.size(item$value))
  })
  names(info) <- keys
  info
}

clear_cache <- function(pattern = NULL) {
  keys <- ls(envir = .cache_env, all.names = TRUE)
  if (!is.null(pattern)) keys <- grep(pattern, keys, value = TRUE)
  if (length(keys) > 0) rm(list = keys, envir = .cache_env)
  invisible(TRUE)
}

# --- Helpers específicos para cache de arquivos (repo .rds) ----------------
# Guardamos uma lista com value + mtime do arquivo
cache_set_file <- function(key, value, filepath) {
  info <- file.info(filepath)
  mtime <- if (is.na(info$mtime)) Sys.time() else info$mtime
  assign(key, list(value = value, mtime = mtime, time = Sys.time()), envir = .cache_env)
  invisible(TRUE)
}

cache_get_file <- function(key, filepath) {
  if (!exists(key, envir = .cache_env, inherits = FALSE)) return(NULL)
  rec <- get(key, envir = .cache_env, inherits = FALSE)
  info <- file.info(filepath)
  if (is.na(info$mtime)) return(NULL) # arquivo não existe mais
  if (!identical(rec$mtime, info$mtime)) {
    # arquivo modificado -> invalida cache
    cache_remove(key)
    return(NULL)
  }
  rec$value
}

# Constrói chave padronizada a partir do caminho absoluto do arquivo
cache_key_from_path <- function(path) {
  paste0("filecache::", normalizePath(path, winslash = "/", mustWork = FALSE))
}

# getters para metadata
get_gemac_series <- function() {
  key <- "meta::gemac_series"
  if (cache_has(key)) return(cache_get(key))
  # tenta localizar no pacote/extdata ou em data/
  path <- "data/gemac_series.rds"
  if (!file.exists(path)) {
    # alternativa: system.file("extdata", "gemac_series.rds", package = "macroseries")
    stop("Arquivo de metadados 'data/gemac_series.rds' não encontrado. Coloque em data/ ou extdata/ ou carregue manualmente.")
  }
  obj <- readRDS(path)
  cache_set(key, obj)
  obj
}

get_names_list <- function() {
  key <- "meta::names_list"
  if (cache_has(key)) return(cache_get(key))
  path <- "data/names_list.rds"
  if (!file.exists(path)) stop("Arquivo 'data/names_list.rds' não encontrado.")
  obj <- readRDS(path)
  cache_set(key, obj)
  obj
}

get_codigos_7060 <- function() {
  key <- "meta::codigos_7060"
  if (cache_has(key)) return(cache_get(key))
  path <- "data/codigos_7060.rds"
  if (!file.exists(path)) return(NULL)
  obj <- readRDS(path)
  cache_set(key, obj)
  obj
}

# helpers para cache local

regra_key <- function(regra) {
  paste0(rec_serie(regra), "::", rec_medida(regra), "::", rec_frequencia(regra), "::", rec_territorio(regra))
}

make_local_cache <- function() {
  env <- new.env(parent = emptyenv())
  env$results <- list()
  env$visited <- character(0)
  env
}