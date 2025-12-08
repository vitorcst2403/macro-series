source("renv/activate.R")

start_gemac <- function() {
  renv::restore()
  
  gemac <- new.env()
  
  for (files in list.files(path = "scripts", 
                           pattern = "\\.R$", 
                           recursive = TRUE, 
                           full.names = TRUE)) {
    source(files, local = gemac)
  }
  
  gemac$gemac_series <- readRDS(file = "data/gemac_series.rds")
  gemac$codigos_7060 <- readRDS(file = "data/codigos_7060.rds")
  gemac$names_tbl <- readRDS(file = "data/names_tbl.rds")
  
  attach(gemac, name = "gemac")
}

start_gemac()
