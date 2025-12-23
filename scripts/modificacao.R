# modificacao.R (substituição)
# Implementa os modificadores (mods) usados pelo sistema e registra
# um mods_registry + mods_aliases prontas para uso pela função consulta().
#
# Objetivos:
# - Definir internamente as funções de transformação (com nomes curtos)
# - Manter comportamento compatível com tratamento de unidades ("%") e frequências
# - Fornecer um `mods_registry` (map name -> function(ms, params)) e `mods_aliases`
#   (map apelido -> canonical string(s)) para integração com consulta.R
#
# AVISO: este arquivo define as funções localmente e expõe `mods_registry`
# e `mods_aliases` no ambiente onde for sourceado. Em um pacote, mova a
# inicialização para registries_env conforme discutido.

# --- helpers -----------------------------------------------------------------

trim <- function(x) gsub("^\\s+|\\s+$", "", x)
`%||%` <- function(a, b) if (!is.null(a)) a else b

# cast de parametros simples (utilizado na criação de aliases parseados externamente)
cast_single <- function(x) {
  if (is.null(x)) return(NULL)
  if (is.character(x)) {
    x <- trim(x)
    if (grepl("^[-+]?[0-9]+$", x)) return(as.integer(x))
    if (grepl("^[-+]?[0-9]*\\.[0-9]+$", x)) return(as.numeric(x))
    if (tolower(x) %in% c("true", "false")) return(tolower(x) == "true")
    if ((startsWith(x, "'") && endsWith(x, "'")) || (startsWith(x, "\"") && endsWith(x, "\""))) {
      return(substr(x, 2, nchar(x) - 1))
    }
    return(x)
  }
  x
}

# retorna inteiro de parâmetros (n, k, h ou _args)
get_int_param <- function(params, keys = c("n", "k", "h")) {
  if (is.null(params)) return(NULL)
  for (k in keys) if (!is.null(params[[k]])) return(as.integer(params[[k]]))
  if (!is.null(params[["_args"]]) && length(params[["_args"]]) >= 1) return(as.integer(params[["_args"]][1]))
  NULL
}

get_str_param <- function(params, keys = c("format", "formato", "fmt")) {
  if (is.null(params)) return(NULL)
  for (k in keys) if (!is.null(params[[k]])) return(as.character(params[[k]]))
  if (!is.null(params[["_args"]]) && length(params[["_args"]]) >= 1) return(as.character(params[["_args"]][1]))
  NULL
}

# --- implementações das transformações (nomes curtos) ------------------------

# média móvel de n períodos (ma)
ma <- function(ms, n) {
  # preserva atributos, trata unidade "%" com transformação geométrica
  attrs <- attributes(ms)
  if (is.null(attrs)) attrs <- list()
  unid <- attrs$unidade %||% rec_unidade(ms)
  x <- as.numeric(ms)
  if (is.null(n) || !is.numeric(n)) stop("ma(): forneça n inteiro, ex: ma(ms, 3)")
  if (unid == "%") {
    f <- function(v) {
      val <- (exp(mean(log(1 + v/100), na.rm = TRUE)) - 1) * 100
      return(val)
    }
  } else {
    f <- function(v) mean(v, na.rm = TRUE)
  }
  res <- zoo::rollapply(x, width = as.integer(n), FUN = f, fill = NA, align = "right")
  attributes(res) <- attrs
  res
}

# acumulado de n períodos (acum)
acum <- function(ms, n) {
  attrs <- attributes(ms)
  unid <- attrs$unidade %||% rec_unidade(ms)
  x <- as.numeric(ms)
  if (is.null(n) || !is.numeric(n)) stop("acum(): forneça n inteiro, ex: acum(ms, 3)")
  if (unid == "%") {
    f <- function(v) {
      val <- (exp(sum(log(1 + v/100), na.rm = TRUE)) - 1) * 100
      return(val)
    }
  } else {
    f <- function(v) sum(v, na.rm = TRUE)
  }
  res <- zoo::rollapply(x, width = as.integer(n), FUN = f, fill = NA, align = "right")
  attributes(res) <- attrs
  res
}

# acumulado do ano (acum_ano)
acum_ano <- function(ms) {
  attrs <- attributes(ms)
  freq <- rec_frequencia(ms)
  if (freq == "D") {
    message("Acumulado no ano não é aplicável a série diária.")
    return(ms)
  }
  unid <- rec_unidade(ms)
  x <- as.numeric(ms)
  ts_time <- time(ms)
  ano <- floor(ts_time)
  df <- data.frame(ano = ano, x = x)
  if (unid == "%") {
    f <- function(v) (exp(cumsum(log(1 + v/100))) - 1) * 100
  } else {
    f <- cumsum
  }
  df <- dplyr::group_by(df, ano)
  df <- dplyr::mutate(df, x = f(x))
  res <- df$x
  attributes(res) <- attrs
  res
}

# acumulado total (acumulado)
acumulado_total <- function(ms) {
  attrs <- attributes(ms)
  unid <- attrs$unidade %||% rec_unidade(ms)
  x <- as.numeric(ms)
  if (unid == "%") {
    res <- (exp(cumsum(log(1 + x/100))) - 1) * 100
  } else {
    res <- cumsum(x)
  }
  attributes(res) <- attrs
  res
}

# anualizado (anualiza)
anualiza <- function(ms) {
  attrs <- attributes(ms)
  unid <- attrs$unidade %||% rec_unidade(ms)
  freq <- attrs$frequencia %||% rec_frequencia(ms)
  if (freq == "D") {
    message("Série diária não pode ser anualizada.")
    return(ms)
  }
  freqs <- c("Q" = 12, "M" = 12, "T" = 4, "S" = 2, "A" = 1)
  x <- as.numeric(ms)
  if (unid == "%") {
    x2 <- ((1 + x/100) ^ freqs[freq] - 1) * 100
  } else {
    x2 <- freqs[freq] * x
  }
  attributes(x2) <- attrs
  x2
}

# shift / lag (shift)
shift <- function(ms, n) {
  freq <- rec_frequencia(ms)
  if (freq == "D") {
    message("shift(): não é aplicável para séries diárias.")
    return(ms)
  }
  x <- stats::lag(ms, n)
  x <- janela(x, inicio = rec_inicio(ms), fim = rec_fim(ms))
  x
}

# datas (formatação de período)
dates_fmt <- function(ms, formato = "%b/%y") {
  attrs <- attributes(ms)
  x <- as.numeric(time(ms))
  attributes(x) <- attrs
  attr(x, "medida") <- "Tempo"
  attr(x, "unidade") <- formato
  x
}

# diferença n períodos (diff)
diff_n <- function(ms, n) {
  attrs <- attributes(ms)
  inicio <- rec_inicio(ms)
  freq <- rec_frequencia(ms)
  # calcula novo inicio avançando n períodos
  inicio_novo <- Reduce(function(acc, ...) prox_periodo(acc, freq), x = seq_len(n), init = inicio)
  x <- abs(diff(ms, lag = n))
  attributes(x) <- c(attributes(x), attrs[setdiff(names(attrs), names(attributes(x)))])
  attr(x, "inicio") <- inicio_novo
  x
}

# --- Cria o mods_registry: mapeia nomes canônicos para handlers -------------
# Cada handler tem assinatura handler(ms, params) — onde params é uma lista já parseada.
mods_registry <- list(
  ma = function(ms, params) {
    n <- get_int_param(params, c("n", "k"))
    if (is.null(n)) stop("ma(): forneça n ex: ma(n=3) ou ma(3)")
    ma(ms, n = as.integer(n))
  },
  acum = function(ms, params) {
    n <- get_int_param(params, c("n"))
    if (is.null(n)) stop("acum(): forneça n ex: acum(n=3) ou acum(3)")
    acum(ms, n = as.integer(n))
  },
  acum_ano = function(ms, params) {
    acum_ano(ms)
  },
  acumulado = function(ms, params) {
    acumulado_total(ms)
  },
  anualizado = function(ms, params) {
    anualiza(ms)
  },
  shift = function(ms, params) {
    n <- get_int_param(params, c("n"))
    if (is.null(n)) stop("shift(): forneça n ex: shift(n=1) ou shift(1)")
    shift(ms, n = as.integer(n))
  },
  lag = function(ms, params) {
    # alias para shift
    n <- get_int_param(params, c("n"))
    if (is.null(n)) stop("lag(): forneça n ex: lag(n=1) ou lag(1)")
    shift(ms, n = as.integer(n))
  },
  datas = function(ms, params) {
    fmt <- get_str_param(params, c("format", "formato", "fmt")) %||% "%b/%y"
    dates_fmt(ms, formato = fmt)
  },
  diff = function(ms, params) {
    n <- get_int_param(params, c("n"))
    if (is.null(n)) stop("diff(): forneça n ex: diff(n=1) ou diff(1)")
    diff_n(ms, n = as.integer(n))
  }
)

# --- Aliases de modificadores (mods_aliases) --------------------------------
# Mapas de apelidos (case-insensitive). Valores podem ser:
# - string "nome(params...)" -> expandido pelo parser
# - vetor de strings c("op1(...)", "op2(...)") -> pipeline de ops
mods_aliases <- list(
  "12 meses"       = c("ma(12)", "anualizado"),
  "mm12"           = c("ma(12)", "anualizado"),
  "mm3m"           = "ma(3)",
  "mm3ma"          = c("ma(3)", "anualizado"),
  "mm3"            = "ma(3)",
  "mm"             = "ma(2)",
  
  "acumulado"      = "acumulado",
  "acumulado ano"  = "acum_ano",
  "acum"           = "acum",
  
  "anualizado"     = "anualizado",
  "12m"            = c("ma(12)", "anualizado"),
  
  "m1"             = "shift(-1)",
  "m4"             = "shift(-4)",
  "m12"            = "shift(-12)",
  "lag1"           = "shift(-1)",
  
  "dia"            = "datas('%d')",
  "mes"            = "datas('%B')",
  "ano"            = "datas('%Y')",
  "mesano"         = "datas('%B de %Y')",
  "diamesano"      = "datas('%d de %B de %Y')",
  "bb"             = "datas('%b')",
  "bbyy"           = "datas('%b/%y')",
  "bbyyyy"         = "datas('%b/%Y')",
  "mmyy"           = "datas('%m/%y')",
  "mmyyyy"         = "datas('%m/%Y')",
  "ddmmyyyy"       = "datas('%d/%m/%Y')",
  
  "d1"             = "diff(1)",
  "d12"            = "diff(12)",
  
  "ma"             = "ma(3)" # exemplo: ma sem parâmetro mapeado para ma(3)
)

# Normaliza chaves para lowercase para lookup case-insensitive
mods_aliases <- stats::setNames(mods_aliases, tolower(names(mods_aliases)))
