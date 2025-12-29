extrai_regra <- function(repo) {
  data_path <- getOption("macroseries.data_path")
  if (is.null(data_path)) {
    stop("Defina o endereço do repo com options(macroseries.data_path = 'J:/meu/repo')")
  }
  
  regra_wd <- file.path(data_path, "regras")
  regra_wd <- file.path(regra_wd,  paste0(repo, ".qs"))
  
  regra_abs <- normalizePath(regra_wd, winslash = "/", mustWork = FALSE)
  if (!file.exists(regra_abs)) {
    message(sprintf("Regra '%s' inexistente no repositório", repo))
    return(NULL)
  }
  
  regra <- qs::qread(file = regra_abs)
  return(regra)
}
