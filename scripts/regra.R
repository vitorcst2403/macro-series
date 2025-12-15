# Regra -----------------------------------------

class_regra <- function(serie,
                        medida = NULL,
                        frequencia = NULL,
                        territorio = NULL) {
  # Cria regra de formação com base no nome da 'serie' e na lista pré definida de séries 
  
  serie = serie[1]
  gemac_series <- get_gemac_series()
  
  if (is.null(gemac_series[[serie]])) {
    stop(sprintf("Série '%s' não encontrada em gemac_series", serie), call. = FALSE)
  }
  
  serie_info <- gemac_series[[serie]]$info
  serie_padrao <- gemac_series[[serie]]$padrao
  
  if (is.null(medida)) {
    medida = serie_padrao[["medida"]]
  }
  
  if (is.null(frequencia)) {
    frequencia = serie_padrao[["frequencia"]]
  }
  
  if (is.null(territorio)) {
    if (is.null(serie_padrao[["territorio"]])) {
      classe <- gemac_series[[serie]][[medida]][[frequencia]]$metodo
      unidade <- gemac_series[[serie]][[medida]][[frequencia]]$unidade
      dados <- gemac_series[[serie]][[medida]][[frequencia]]$dados
      inicio <- gemac_series[[serie]][[medida]][[frequencia]]$inicio
      
      att_list <- list(
        class = classe,
        unidade = unidade,
        inicio = inicio,
        medida = medida,
        frequencia = frequencia
      )
      
      att_list <- c(serie_info, att_list)
      
      obj <- structure(.Data = dados, .Names = NULL)
      attributes(obj) <- c(attributes(obj), att_list)
      
      return(obj)
    }
    
    territorio = serie_padrao[["territorio"]]
  }
  
  classe <- gemac_series[[serie]][[medida]][[frequencia]][[territorio]]$metodo
  if (is.null(classe)) {
    return(NULL)
  }
  unidade <- gemac_series[[serie]][[medida]][[frequencia]][[territorio]]$unidade
  if (is.null(unidade)) {
    return(NULL)
  }
  dados <- gemac_series[[serie]][[medida]][[frequencia]][[territorio]]$dados
  if (is.null(dados)) {
    return(NULL)
  }
  inicio <- gemac_series[[serie]][[medida]][[frequencia]][[territorio]]$inicio
  if (is.null(inicio)) {
    return(NULL)
  }
  
  att_list <- list(
    serie = serie,
    class = classe,
    unidade = unidade,
    inicio = inicio,
    medida = medida,
    frequencia = frequencia,
    territorio = territorio
  )
  
  att_list <- c(serie_info, att_list)
  
  obj <- structure(.Data = dados)
  attributes(obj) <- c(attributes(obj), att_list)
  
  return(obj)
}

attr_regra <- function(ms, regra) {
  attr(ms, "tema") <- rec_tema(regra)
  attr(ms, "descricao") <- rec_descricao(regra)
  attr(ms, "serie") <- rec_serie(regra)
  attr(ms, "unidade") <- rec_unidade(regra)
  attr(ms, "medida") <- rec_medida(regra)
  attr(ms, "territorio") <- rec_territorio(regra)
  
  return(ms)
}

# cria funções que extraem os componentes da regra ou serie

atribs <- c("serie", "unidade", "inicio", "fim", "medida", "frequencia", "territorio", "tema", "descricao")
# indicar wrappers como funções (ao invés de strings)
funs <- list(inicio = as.Date, fim = as.Date)

# Factory robusta que cria funções rec_<atrib> fechando os valores corretos
make_rec <- function(atrib, wrapper = NULL, envir = parent.frame()) {
  # Usamos local(...) para capturar (fixar) os valores de 'atrib' e 'wrapper'
  if (!is.null(wrapper)) {
    f <- local({
      a <- atrib
      w <- wrapper
      function(x) w(attr(x, a))
    })
  } else {
    f <- local({
      a <- atrib
      function(x) attr(x, a)
    })
  }
  # garante que a função tem um argumento 'x'
  formals(f) <- alist(x = )
  # NÃO sobrescrever o ambiente da função: mantemos a closure criada pelo local()
  assign(paste0("rec_", atrib), f, envir = envir)
}

# cria as funções rec_... no ambiente global (ou outro envir passado)
for (atrib in atribs) {
  wrapper <- if (!is.null(funs[[atrib]])) funs[[atrib]] else NULL
  make_rec(atrib, wrapper, envir = gemac)
}