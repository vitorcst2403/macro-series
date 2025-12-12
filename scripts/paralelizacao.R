# determine plan and workers
choose_future_plan <- function(workers = NULL, prefer = c("multisession", "multicore")) {
  if (is.null(workers)) {
    workers <- max(1, parallel::detectCores(logical = FALSE) - 1)
  }
  # choose plan: multisession is safe on Windows
  if (.Platform$OS.type == "windows") {
    future::plan(future::multisession, workers = workers)
  } else {
    # multicore slightly faster on Unix but not available in RStudio server sometimes
    future::plan(future::multisession, workers = workers)
  }
  invisible(NULL)
}
