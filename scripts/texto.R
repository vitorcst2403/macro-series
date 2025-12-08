texto <- function(conteudo) {
  ultimo_dado <- function(nome) {
    ms <- consulta(nome)
    medi <- rec_medida(ms)
    
    if(medi == "Tempo") {
      unid <- rec_unidade(ms)
      freq <- rec_frequencia(ms)
      
      valor <- tail(ms, 1)
      
      if(freq == "D") {
        valor <- as.Date(as.numeric(tail(ms, 1)), origin = "1970-01-01")
      } else {
        ano <- floor(valor)
        freqs <- c("Q" = 24,
                   "M" = 12,
                   "T" = 4,
                   "S" = 2,
                   "A" = 1)
        mes <- round((valor - ano)*freqs[freq])+1
        if (freq == "Q") mes = floor(mes/2)
        valor <- as.Date(paste(ano, mes, "01", sep = "-"))
      }
      
      final <- format(valor, format = unid)
      return(final)
    }
    
    unid <- rec_unidade(ms)
    valor <- format(round(tail(ms, 1), 2), decimal.mark = ",")
    final <- paste0(valor, unid)
    
    return(final)
  }
  
  matches <- gregexpr("`([^`]*)`", conteudo, perl = TRUE)
  keys <- regmatches(conteudo, matches)[[1]]
  clean_keys <- gsub("`", "", keys)
  replacements <- sapply(clean_keys, ultimo_dado)
  
  for (i in seq_along(keys)) {
    conteudo <- sub(keys[i], replacements[i], conteudo, fixed = TRUE)
  }
  
  return(conteudo)
}
