texto <- function(conteudo) {
  # Função que processa texto com códigos entre `backticks` substituindo pelo último dado
  ultimo_dado <- function(nome) {
    ms <- consulta(nome)
    medi <- rec_medida(ms)
    
    if (medi == "Tempo") {
      unid <- rec_unidade(ms)
      freq <- rec_frequencia(ms)
      
      valor <- tail(ms, 1)
      
      if (freq == "D") {
        valor <- as.Date(as.numeric(tail(ms, 1)), origin = "1970-01-01")
      } else {
        ano <- floor(valor)
        freqs <- c("Q" = 24,
                   "M" = 12,
                   "T" = 4,
                   "S" = 2,
                   "A" = 1)
        mes <- round((valor - ano) * freqs[freq]) + 1
        if (freq == "Q") mes <- floor(mes / 2)
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
  
  # encontra todas as chaves {{...}} e substitui
  pattern <- "\\{\\{([^}]+)\\}\\}"
  matches <- gregexpr(pattern, conteudo, perl = TRUE)
  raw_keys <- regmatches(conteudo, matches)[[1]]
  
  if (length(raw_keys) == 0 || all(raw_keys == "")) {
    processed <- conteudo
  } else {
    # extrai chaves limpas (conteúdo dentro das chaves)
    clean_keys <- vapply(raw_keys, function(k) {
      # remove {{ e }}
      substr(k, 3, nchar(k) - 2)
    }, FUN.VALUE = character(1))
    
    # obtém substituições com tratamento de erro
    replacements <- vapply(clean_keys, function(k) {
      k_trim <- trimws(k)
      tryCatch(
        ultimo_dado(k_trim),
        error = function(e) {
          warning("texto(): erro ao obter '", k_trim, "': ", e$message)
          paste0("<erro:", k_trim, ">")
        }
      )
    }, FUN.VALUE = character(1))
    
    # substitui sequencialmente (cada raw_keys pode ter caracteres especiais)
    processed <- conteudo
    for (i in seq_along(raw_keys)) {
      processed <- sub(raw_keys[i], replacements[i], processed, fixed = TRUE)
    }
  }
  
  # retorna um objeto com classe especial para controlar impressão em chunk vs inline
  structure(processed, class = c("texto_output", class(processed)))
}

# Método para impressão em chunks (knitr irá usar knit_print)
knit_print.texto_output <- function(x, ...) {
  # x é a string já processada; imprimimos como asis com duas quebras de linha
  if (requireNamespace("knitr", quietly = TRUE)) {
    knitr::asis_output(paste0(as.character(x), "\n\n"))
  } else {
    # fallback: mostrar no console (útil fora do knit)
    cat(as.character(x), "\n\n")
    invisible()
  }
}

# print: usado no console — imprime texto limpo (sem aspas, sem mostrar atributos)
print.texto_output <- function(x, ...) {
  cat(as.character(x), "\n", sep = "")
  invisible(x)
}

# Garantir que inline use apenas a string (sem quebras adicionais)
format.texto_output <- function(x, ...) {
  as.character(x)
}
as.character.texto_output <- function(x, ...) {
  unclass(x)[1]
}