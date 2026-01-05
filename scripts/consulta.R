# consulta.R (versão atualizada)
# DSL baseado em delimitadores %m% (mod), %f% (fil), %p% (proj)
#
# Observações importantes:
# - Esta função NÃO registra handlers. Espera-se que os registries e aliases
#   estejam definidos externamente como:
#     mods_registry     (lista de funções: name -> function(ms, params))
#     filters_registry  (lista de funções: name -> function(ms, params))
#     proj_registry     (lista de funções: name -> function(ms, params))
#     mods_aliases      (lista: apelido -> "canonical(params...)" string)
#     filters_aliases
#     proj_aliases
#   Ex.: mods_aliases[['12 meses']] <- "ma(12)"
# - Busca por nomes de séries é case-insensitive (ipca == IPCA).
# - Aliases também são resolvidos case-insensitivamente.
# - Parâmetros fornecidos diretamente na expressão têm precedência sobre os do alias.
#
# Exemplo:
#   consulta("IPCA %m% 12 meses %f% saz %p% arma(1,0,1)")
#   # se mods_aliases[['12 meses']] == "ma(12)" -> expande para ma(12)

consulta <- function(...,
                     inicio = NULL,
                     fim = NULL,
                     atualiza = FALSE) {
  opcao_original <- getOption("macroseries.atualiza", FALSE)
  if (atualiza) {
    options(macroseries.atualiza = TRUE)
    on.exit(options(macroseries.atualiza = opcao_original), add = TRUE)
  }
  atual <- getOption("macroseries.atualiza")
  
  # ---------- utilitários ----------
  trim <- function(x) gsub("^\\s+|\\s+$", "", x)
  
  cast_type <- function(v) {
    v <- trim(v)
    if (v == "") return("")
    if (grepl("^[-+]?[0-9]+$", v)) return(as.integer(v))
    if (grepl("^[-+]?[0-9]*\\.[0-9]+$", v)) return(as.numeric(v))
    if (tolower(v) %in% c("true", "false")) return(tolower(v) == "true")
    if ((startsWith(v, "'") && endsWith(v, "'")) || (startsWith(v, "\"") && endsWith(v, "\""))) {
      return(substring(v, 2, nchar(v) - 1))
    }
    return(v)
  }
  
  parse_params <- function(s) {
    s <- trim(s)
    if (is.null(s) || s == "") return(list())
    parts <- strsplit(s, ",")[[1]]
    res <- list()
    for (p in parts) {
      p <- trim(p)
      if (p == "") next
      if (grepl("=", p, fixed = TRUE)) {
        kv <- strsplit(p, "=", fixed = TRUE)[[1]]
        k <- trim(kv[1]); v <- cast_type(kv[2])
        res[[k]] <- v
      } else {
        res[["_args"]] <- c(res[["_args"]], cast_type(p))
      }
    }
    res
  }
  
  # parse expressão: série %m% op(params) %f% op2(...) ...
  # substitua a função parse_expression atual por esta versão corrigida
  parse_expression <- function(expr) {
    expr_orig <- expr
    expr <- as.character(expr)
    # encontra primeiro token (%m% / %f% / %p%)
    mpos <- regexpr("%[mfp]%", expr, perl = TRUE)
    if (mpos == -1) {
      serie <- trimws(expr)
      return(list(serie = serie, ops = list()))
    }
    serie <- trimws(substr(expr, 1, mpos - 1))
    rest <- substr(expr, mpos, nchar(expr))
    ops <- list()
    cur <- rest
    
    while (nchar(cur) > 0) {
      # cur deve começar com token
      m <- regexpr("^%([mfp])%", cur, perl = TRUE)
      if (m != 1) {
        stop(sprintf("Erro de sintaxe na expressão: '%s' (esperado token %%m%%/%%f%%/%%p%%)", expr_orig))
      }
      prefix_letter <- substring(cur, 2, 2) # "m", "f" ou "p"
      prefix <- ifelse(prefix_letter == "m", "mod", ifelse(prefix_letter == "f", "fil", "proj"))
      # remove token
      cur <- substring(cur, 4)
      cur <- trimws(cur)
      
      # localizar próximo token (se existir) para saber o limite deste segmento
      next_token_pos <- regexpr("%[mfp]%", cur, perl = TRUE)
      if (next_token_pos == -1) {
        segment <- cur
        remainder <- ""
      } else {
        segment <- substr(cur, 1, next_token_pos - 1)
        remainder <- substr(cur, next_token_pos, nchar(cur))
      }
      
      # agora parsear 'segment' que pode ser: "nome" ou "nome(params...)"
      # regex: captura tudo antes do '(' como nome (inclui espaços), opcionalmente captura o conteúdo dos parênteses
      m2 <- regexec("^\\s*([^\\(]+?)\\s*(?:\\((.*)\\))?\\s*$", segment, perl = TRUE)
      mm <- regmatches(segment, m2)[[1]]
      if (length(mm) == 0) {
        stop(sprintf("Operador inválido ou mal formatado após token %%%s%% em '%s' (segmento: '%s')",
                     prefix_letter, expr_orig, segment))
      }
      op_name_raw <- trimws(mm[2])
      op_params_raw <- if (length(mm) >= 3) mm[3] else ""
      params <- parse_params(op_params_raw)
      
      ops[[length(ops) + 1]] <- list(prefix = prefix,
                                     name_raw = op_name_raw,
                                     params = params)
      
      # avança para o remainder
      cur <- trimws(remainder)
    }
    
    list(serie = serie, ops = ops)
  }
  
  # lowercased alias lookup maps
  lc_map <- function(lst) {
    if (length(lst) == 0) return(list())
    res <- list()
    for (k in names(lst)) res[[tolower(k)]] <- lst[[k]]
    res
  }
  mods_aliases_lc <- lc_map(mods_aliases)
  #filters_aliases_lc <- lc_map(filters_aliases)
  #proj_aliases_lc <- lc_map(proj_aliases)
  
  parse_canonical <- function(canonical_str) {
    # canonical_str example: "ma(12)" or "saz" or "arma(1,0,1)"
    s <- trim(canonical_str)
    m <- regexec("^([A-Za-z0-9_\\.\\-]+)\\s*(?:\\((.*)\\))?\\s*$", s, perl = TRUE)
    mm <- regmatches(s, m)[[1]]
    if (length(mm) == 0) stop(sprintf("Alias canônico inválido: '%s'", s))
    name <- tolower(mm[2])
    params <- parse_params(ifelse(length(mm) >= 3, mm[3], ""))
    list(name = name, params = params)
  }
  
  resolve_alias <- function(prefix, name_raw, params_given) {
    # prefix in c("mod","fil","proj"), name_raw arbitrary string
    name_lc <- tolower(trim(name_raw))
    alias_val <- NULL
    if (prefix == "mod") alias_val <- mods_aliases_lc[[name_lc]]
    if (prefix == "fil") alias_val <- filters_aliases_lc[[name_lc]]
    if (prefix == "proj") alias_val <- proj_aliases_lc[[name_lc]]
    if (!is.null(alias_val)) {
      canonical <- parse_canonical(as.character(alias_val))
      # merge params: explicit params override alias params
      merged <- canonical$params
      for (nm in names(params_given)) merged[[nm]] <- params_given[[nm]]
      return(list(name = canonical$name, params = merged))
    } else {
      # no alias: attempt to interpret name_raw as canonical name possibly with spaces (e.g. "12 meses" without alias)
      # normalize name (no params)
      return(list(name = tolower(gsub("\\s+", " ", trim(name_raw))), params = params_given))
    }
  }
  
  # ---------- parse entradas ----------
  exprs <- list(...)
  if (length(exprs) == 0) stop("Forneça ao menos uma expressão de consulta.")
  exprs <- lapply(exprs, as.character)
  parsed <- lapply(exprs, parse_expression)
  series_names_input <- tolower(vapply(parsed, function(x) x$serie, character(1)))
  args <- lista_repos[series_names_input]
  
  # extrai as séries
  ms_list <- lapply(args, function(repos) {
      extrai_safe(repos, 
                  inicio = inicio,
                  atual = atual)
  })
  
  return(ms_list)
  
  # aplica pipeline para cada parsed item
  apply_parsed <- function(parsed_item) {
    sname <- parsed_item$serie
    ms <- ms_list[[sname]]
    if (is.null(ms)) return(NULL)
    ops <- parsed_item$ops
    for (op in ops) {
      prefix <- op$prefix
      name_raw <- op$name_raw
      params_given <- op$params
      resolved <- resolve_alias(prefix, name_raw, params_given)
      name <- resolved$name
      params <- resolved$params
      # lookup handler in registries (they must be defined externally)
      if (prefix == "mod") {
        if (is.null(mods_registry)) stop("mods_registry não definido. Carregue o script que registra os modificadores.")
        handler <- mods_registry[[name]]
        if (is.null(handler)) stop(sprintf("Modificador não encontrado: '%s' (sugerido a partir de '%s')", name, name_raw))
        ms <- handler(ms, params)
      } else if (prefix == "fil") {
        if (is.null(filters_registry)) stop("filters_registry não definido. Carregue o script que registra os filtros.")
        handler <- filters_registry[[name]]
        if (is.null(handler)) stop(sprintf("Filtro não encontrado: '%s' (a partir de '%s')", name, name_raw))
        ms <- handler(ms, params)
      } else if (prefix == "proj") {
        if (is.null(proj_registry)) stop("proj_registry não definido. Carregue o script que registra as projeções.")
        handler <- proj_registry[[name]]
        if (is.null(handler)) stop(sprintf("Projeção não encontrada: '%s' (a partir de '%s')", name, name_raw))
        ms <- handler(ms, params)
      } else {
        stop(sprintf("Prefixo inválido: '%s'", prefix))
      }
    }
    ms
  }
  
  ms_applied <- lapply(parsed, apply_parsed)
  if (length(ms_applied) == 0) return(NULL)
  ms_comb <- Reduce(function(x, y) combina_serie(x, y, fill = FALSE), ms_applied)
  ms_comb <- janela(ms_comb, inicio = inicio, fim = fim)
  return(ms_comb)
}