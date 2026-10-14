#' Reference text for a dataset
#'
#' Write the reference of datasets from their metadata (see [ndc_metadata()]): the citation (`sci:citation`), the
#' link of the DOI (`sci:doi`) if the citation does not contain it already, and the license.
#'
#' @param collections character. IDs of the collections.
#' @param metadata tibble. The result of [ndc_metadata()], if it was read already (otherwise it is requested).
#' @param license boolean. If `TRUE`, add the license.
#' @param publications boolean. If `TRUE`, add the citations of the related publications (`sci:publications`).
#' @param token character. API token.
#' @returns A character vector with one text per collection, named after the collection. The text is `NA` for a
#'   collection without citation.
#' @examples
#' \dontrun{
#' ndc_citation(c("lgn", "ntot"))
#' }
#' @export
ndc_citation <- function(collections, metadata = NULL, license = TRUE, publications = FALSE,
                         token = Sys.getenv("NDC_TOKEN")) {
  collections <- as.character(collections)
  if (is.null(metadata)) metadata <- ndc_metadata(unique(collections), token = token)
  unknown <- setdiff(collections, metadata$id)
  if (length(unknown) > 0) stop("Unknown collection(s): ", paste(unknown, collapse = ", "), ".", call. = FALSE)

  text <- vapply(collections, function(id) {
    m <- metadata[match(id, metadata$id), ]
    if (is.na(m$citation)) return(NA_character_)
    parts <- trimws(m$citation)
    if (!is.na(m$doi) && !grepl(m$doi, parts, fixed = TRUE)) parts <- paste0(parts, " https://doi.org/", m$doi)
    if (license && !is.na(m$license)) parts <- paste0(parts, " License: ", m$license, ".")
    pubs <- m$publications[[1]]$citation
    pubs <- pubs[!is.na(pubs)]
    if (publications && length(pubs) > 0) parts <- paste0(parts, "\nRelated: ", paste(pubs, collapse = "\nRelated: "))
    parts
  }, "")
  stats::setNames(text, collections)
}
