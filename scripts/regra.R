# Regra -----------------------------------------

class_regra <- function(serie,
                        medida = NULL,
                        frequencia = NULL,
                        territorio = NULL) {
  # Cria regra de formação com base no nome da 'serie' e na lista pré definida de séries 
  
  serie = serie[1]
  
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
funs <- list(inicio = "as.Date", fim = "as.Date")

for(atrib in atribs) {
  wrapper <- if (!is.null(funs[[atrib]])) funs[[atrib]] else NULL
  
  if(!is.null(wrapper)) {
    code <- sprintf(
      'rec_%s <- function(x) { %s(attr(x, "%s")) }',
      atrib, wrapper, atrib
    )
    eval(parse(text = code))
  } else {
    code <- sprintf(
      'rec_%s <- function(x) { attr(x, "%s") }',
      atrib, atrib
    )
    eval(parse(text = code))
  }
}


