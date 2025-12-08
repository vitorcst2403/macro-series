# Repositório -----------------------------
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
