# Run the code cells of the example notebooks in tests/ as plain R scripts, stopping at the first error.
# Requires network access and the NDC_TOKEN and ADC_TOKEN environment variables.
#   Rscript tools/run_notebooks.R                    # all notebooks
#   Rscript tools/run_notebooks.R examples_gm        # selected notebooks
# The working tree is loaded with devtools::load_all(), so the installed rNDC is not used.

suppressMessages({ library(jsonlite); library(devtools) })
if (!all(nzchar(Sys.getenv(c("NDC_TOKEN", "ADC_TOKEN"))))) {
  stop("Set the NDC_TOKEN and ADC_TOKEN environment variables.", call. = FALSE)
}

root <- normalizePath(".")
load_all(root, quiet = TRUE, attach_testthat = FALSE)

run_notebook <- function(path) {
  cells <- fromJSON(path, simplifyVector = FALSE)$cells
  code <- Filter(function(c) c$cell_type == "code", cells)
  old <- setwd(dirname(path)); on.exit(setwd(old))
  grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
  env <- new.env(parent = globalenv())  # fresh workspace for each notebook
  for (i in seq_along(code)) {
    src <- paste(unlist(code[[i]]$source), collapse = "")
    tryCatch(
      withVisible(eval(parse(text = src), envir = env)),
      error = function(e) stop(sprintf("%s, code cell %d: %s", basename(path), i, conditionMessage(e)), call. = FALSE)
    )
  }
  cat(sprintf("OK  %s (%d code cells)\n", basename(path), length(code)))
}

args <- commandArgs(trailingOnly = TRUE)
notebooks <- list.files(file.path(root, "tests"), pattern = "\\.ipynb$", full.names = TRUE)
if (length(args)) notebooks <- notebooks[tools::file_path_sans_ext(basename(notebooks)) %in% args]
for (nb in notebooks) run_notebook(nb)
