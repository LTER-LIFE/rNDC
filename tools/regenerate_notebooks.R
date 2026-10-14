# Re-evaluate the code cells of the example notebooks in examples/ with {evaluate} and save the notebooks with the new
# outputs (text, messages, plots), keeping the cells as they are. IRkernel is not installed in the conda environment, so this
# is how the saved outputs are made. A notebook is only written when all its cells ran without an error.
#   Rscript tools/regenerate_notebooks.R                    # all notebooks
#   Rscript tools/regenerate_notebooks.R 02_rasters 04_ndvi # selected notebooks (file names without extension)
# Needs network access and the NDC_TOKEN and ADC_TOKEN environment variables, and the packages jsonlite, evaluate, base64enc.
# The working tree is loaded with devtools::load_all(), so the installed rNDC is not used. Execution counts are 1, 2, ...;
# a notebook is not written if a token, or "Bearer ", appears in the output of a cell.

suppressMessages({ library(jsonlite); library(devtools) })
if (!all(nzchar(Sys.getenv(c("NDC_TOKEN", "ADC_TOKEN"))))) {
  stop("Set the NDC_TOKEN and ADC_TOKEN environment variables.", call. = FALSE)
}
root <- normalizePath(".")
load_all(root, quiet = TRUE, attach_testthat = FALSE)
tokens <- Sys.getenv(c("NDC_TOKEN", "ADC_TOKEN"))

lines_of <- function(x) as.list(unlist(strsplit(paste(x, collapse = ""), "(?<=\n)", perl = TRUE)))

# a recorded plot as the output of a notebook cell (840 x 600 pixels, like the ones of IRkernel)
png_output <- function(plot) {
  file <- tempfile(fileext = ".png")
  on.exit(unlink(file))
  grDevices::png(file, width = 840, height = 600, res = 120)
  grDevices::replayPlot(plot)
  grDevices::dev.off()
  list(output_type = "display_data", metadata = structure(list(), names = character(0)),
       data = list(`image/png` = gsub("\n", "", base64enc::base64encode(file)), `text/plain` = list("plot")))
}

# the outputs of one cell: the text that it printed, its messages and warnings, and its plots
cell_outputs <- function(evaluated, where) {
  outputs <- list()
  stream <- list(name = NULL, text = character())
  flush <- function() {
    if (length(stream$text)) outputs[[length(outputs) + 1]] <<- list(output_type = "stream", name = stream$name, text = lines_of(stream$text))
    stream <<- list(name = NULL, text = character())
  }
  add <- function(name, text) { if (!identical(stream$name, name)) flush(); stream$name <<- name; stream$text <<- c(stream$text, text) }
  for (e in evaluated) {
    if (inherits(e, "error")) stop(where, ": ", conditionMessage(e), call. = FALSE)
    else if (inherits(e, "recordedplot")) { flush(); outputs[[length(outputs) + 1]] <- png_output(e) }
    else if (inherits(e, "message")) add("stderr", conditionMessage(e))
    else if (inherits(e, "warning")) add("stderr", paste0("Warning message:\n", conditionMessage(e), "\n"))
    else if (is.character(e)) add("stdout", e)
  }
  flush()
  outputs
}

regenerate <- function(path) {
  nb <- fromJSON(path, simplifyVector = FALSE)
  env <- new.env(parent = globalenv())   # a fresh workspace for each notebook
  old <- setwd(dirname(path)); on.exit(setwd(old))
  count <- 0L
  for (i in seq_along(nb$cells)) {
    if (nb$cells[[i]]$cell_type != "code") next
    count <- count + 1L
    where <- sprintf("%s, code cell %d", basename(path), count)
    grDevices::pdf(NULL); grDevices::dev.control("enable")   # a device that draws nowhere, and records the plots
    evaluated <- evaluate::evaluate(paste(unlist(nb$cells[[i]]$source), collapse = ""), envir = env, new_device = FALSE,
                                    stop_on_error = 2L)
    grDevices::dev.off()
    outputs <- cell_outputs(evaluated, where)
    text <- as.character(toJSON(outputs, auto_unbox = TRUE))
    if (grepl("Bearer ", text, fixed = TRUE) || any(vapply(tokens, grepl, NA, x = text, fixed = TRUE))) {
      stop("credential-like text in the output of ", where, ": not written", call. = FALSE)
    }
    nb$cells[[i]]$outputs <- outputs
    nb$cells[[i]]$execution_count <- count
  }
  json <- toJSON(nb, auto_unbox = TRUE, pretty = 1, null = "null", digits = NA)
  if (any(vapply(tokens, grepl, NA, x = json, fixed = TRUE))) stop("a token is in ", basename(path), ": not written", call. = FALSE)
  write(json, path)
  cat(sprintf("OK  %s (%d code cells)\n", basename(path), count))
}

args <- commandArgs(trailingOnly = TRUE)
notebooks <- list.files(file.path(root, "examples"), pattern = "\\.ipynb$", full.names = TRUE, recursive = TRUE)
if (length(args)) notebooks <- notebooks[tools::file_path_sans_ext(basename(notebooks)) %in% args]
for (nb in notebooks) regenerate(nb)
