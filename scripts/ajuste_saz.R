# Ajuste sazonal ---------------------------------------------------------

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

sa_series <- function(ms, sa, atual = TRUE) {
  if(!sa) {
    return(ms)
  }
  
  serie <- rec_serie(ms)
  medida <- rec_medida(ms)
  frequencia <- rec_frequencia(ms)
  territorio <- rec_territorio(ms)
  
  regra <- class_regra(
    serie,
    medida = medida,
    frequencia = frequencia,
    territorio = territorio
  )
  
  if(!atual) {
    ms <- extrai_repo(regra, sa = TRUE)
    
    return(ms)
  }
  
  ms <- ajuste_saz(ms)
  salva_repo(ms, regra, sa = TRUE)
  return(ms)
}
