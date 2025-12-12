# Funções locais para renderizar Rmd para .docx usando docs/reference.docx
# Use: o template docs/template.Rmd já chama a função via knit inline.
# Também há uma função para renderizar o documento ativo do RStudio.

render_relatorio <- function(rmd, referencia_docx = NULL, arquivo_saida = NULL, quiet = TRUE) {
  # salva opções atuais
  old_opts <- knitr::opts_chunk$get()
  
  # define opções desejadas apenas durante este render
  knitr::opts_chunk$set(
    echo = FALSE,
    results = "asis",
    message = FALSE,
    warning = FALSE,
    fig.width = 6,
    fig.height = 3
  )
  
  # garante restauração ao final, mesmo em caso de erro
  on.exit(do.call(knitr::opts_chunk$set, old_opts), add = TRUE)
  
  # chama render normalmente (ex.: usa reference_docx do pacote se NULL)
  if (is.null(referencia_docx)) {
    referencia_docx <- system.file("templates", "reference.docx", package = "macroseries")
  }
  rmarkdown::render(
    input = rmd,
    output_format = rmarkdown::word_document(reference_docx = referencia_docx),
    output_file = arquivo_saida,
    quiet = quiet,
    envir = new.env(parent = globalenv())
  )
}

# Renderiza um Rmd para .docx usando o template local em docs/reference.docx
render_relatorio_local <- function(rmd,
                                   referencia_docx = "docs/reference.docx",
                                   arquivo_saida = NULL,
                                   quiet = TRUE) {
  if (missing(rmd) || is.null(rmd) || !nzchar(rmd)) {
    stop("Forneça o caminho do arquivo .Rmd para render_relatorio_local().")
  }
  if (!file.exists(rmd)) stop("Arquivo .Rmd não encontrado: ", rmd)
  if (is.null(arquivo_saida)) arquivo_saida <- sub("\\.[Rr]md$", ".docx", basename(rmd))

  referencia_docx <- normalizePath(referencia_docx, mustWork = FALSE)
  if (!file.exists(referencia_docx)) {
    stop("Arquivo de referência .docx não encontrado em: ", referencia_docx,
         "\nColoque seu template em docs/reference.docx ou passe outro caminho.")
  }

  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    stop("Pacote 'rmarkdown' necessário. Instale com install.packages('rmarkdown').")
  }

  message("Renderizando:\n  Rmd: ", rmd,
          "\n  Template: ", referencia_docx,
          "\n  Saída: ", arquivo_saida)

  rmarkdown::render(
    input = rmd,
    output_format = rmarkdown::word_document(reference_docx = referencia_docx),
    output_file = arquivo_saida,
    quiet = quiet,
    envir = new.env(parent = globalenv())
  )

  caminho <- normalizePath(file.path(dirname(rmd), arquivo_saida))
  message("Concluído: ", caminho)
  invisible(caminho)
}

# Função para ser chamada pelo knit do template (delegadora)
knit_para_template_local <- function(input, ...) {
  # input: caminho do Rmd (fornecido pelo rmarkdown ao chamar knit)
  render_relatorio_local(rmd = input, ...)
}

# Função prática para o usuário: renderiza o documento ativo no RStudio
render_relatorio_ativo <- function() {
  if (!requireNamespace("rstudioapi", quietly = TRUE) || !rstudioapi::isAvailable()) {
    stop("rstudioapi não disponível. Abra o RStudio ou use render_relatorio_local(caminho).")
  }
  doc <- rstudioapi::getActiveDocumentContext()
  if (is.null(doc$path) || doc$path == "") {
    stop("Salve o documento Rmd antes de executar render_relatorio_ativo().")
  }
  # garante que documento salvo
  if (doc$modified) rstudioapi::documentSave(id = doc$id)
  render_relatorio_local(rmd = doc$path)
}