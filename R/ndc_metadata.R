#' Metadata of the datasets
#'
#' Read the metadata that the NatureDataCube publishes for its datasets (STAC collections): title, description,
#' license, keywords, providers, spatial and temporal extent, and the fields of the STAC extensions that the
#' collections use, such as the Scientific extension (`sci:citation`, `sci:doi`, `sci:publications`). The metadata
#' belongs to the collection: the items of a collection do not repeat it, so to cite the data of an item, use the
#' metadata of its collection.
#'
#' All the collections are read with one request. Not every collection has every field: what is missing is `NA`
#' (or an empty table for `providers` and `publications`).
#'
#' @param collections character. IDs of the collections. By default all of them.
#' @param token character. API token.
#' @returns A tibble with one row per collection, with the columns
#'   \itemize{
#'     \item `id`, `title`, `description`, `license`: character;
#'     \item `keywords`: list-column with character vectors;
#'     \item `xmin`, `ymin`, `xmax`, `ymax`: numeric, the spatial extent (EPSG:4326);
#'     \item `start`, `end`: POSIXct, the temporal extent (`NA` for an open end);
#'     \item `doi`, `citation`: character, from `sci:doi` and `sci:citation`;
#'     \item `publications`: list-column with a tibble (`doi`, `citation`) per collection, from `sci:publications`;
#'     \item `providers`: list-column with a tibble (`name`, `description`, `roles`, `url`) per collection;
#'     \item `extensions`: list-column with the URLs of the STAC extensions that the collection declares;
#'     \item one list-column for every other field with a prefix, such as `proj:epsg`, named after the field (it
#'       holds the value as the API gives it, or `NULL`). They are found in the data, so the fields of an extension that
#'       the API starts to use appear without a change of the package.
#'   }
#' @seealso [ndc_citation()] to write a reference, [ndc_datasets()] for the IDs of the collections.
#' @examples
#' \dontrun{
#' meta <- ndc_metadata()
#' meta[, c("id", "title", "license", "doi")]
#' ndc_metadata("lgn")$publications[[1]]
#' }
#' @export
ndc_metadata <- function(collections = NULL, token = Sys.getenv("NDC_TOKEN")) {
  check_token(token, "NatureDataCube", "NDC_TOKEN")
  headers <- add_headers("Authorization" = paste0("Bearer ", token))
  docs <- (rstac::collections(stac(ndc_endpoint())) |> get_request(headers))$collections

  if (!is.null(collections)) {
    collections <- unique(as.character(collections))
    ids <- vapply(docs, function(x) as.character(x$id), "")
    unknown <- setdiff(collections, ids)
    if (length(unknown) > 0) {
      stop("Unknown collection(s): ", paste(unknown, collapse = ", "), ". Available: ", paste(ids, collapse = ", "),
           ".", call. = FALSE)
    }
    docs <- docs[match(collections, ids)]
  }

  meta_rows(docs)
}

# Fields that the table has a column of its own for; the other prefixed fields get a list-column each
meta_known_fields <- c("sci:doi", "sci:citation", "sci:publications")

# A single character value, or NA
meta_text <- function(x) {
  if (is.null(x) || length(x) == 0 || is.list(x)) NA_character_ else as.character(x[[1]])
}

# A time (ISO text) as POSIXct, or NA
meta_time <- function(x) {
  if (is.null(x)) return(as.POSIXct(NA, tz = "UTC"))
  as.POSIXct(lubridate::as_datetime(as.character(x)), tz = "UTC")
}

# The table (doi, citation) of the `sci:publications` of a collection
meta_publications <- function(x) {
  if (is.null(x) || length(x) == 0) return(tibble::tibble(doi = character(), citation = character()))
  if (!is.null(names(x))) x <- list(x)  # a single publication, not a list of them
  tibble::tibble(doi = vapply(x, function(p) meta_text(p$doi), ""),
                 citation = vapply(x, function(p) meta_text(p$citation), ""))
}

# The table (name, description, roles, url) of the providers of a collection
meta_providers <- function(x) {
  empty <- tibble::tibble(name = character(), description = character(), roles = character(), url = character())
  if (is.null(x) || length(x) == 0) return(empty)
  tibble::tibble(name = vapply(x, function(p) meta_text(p$name), ""),
                 description = vapply(x, function(p) meta_text(p$description), ""),
                 roles = vapply(x, function(p) paste(unlist(p$roles), collapse = ", "), ""),
                 url = vapply(x, function(p) meta_text(p$url), ""))
}

meta_rows <- function(docs) {
  n <- length(docs)
  field <- function(f) lapply(docs, function(x) x[[f]])
  bbox <- lapply(docs, function(x) {
    b <- unlist(x$extent$spatial$bbox[[1]])
    if (length(b) == 6) b <- b[c(1, 2, 4, 5)]  # a 3D box: x, y, z, x, y, z
    if (length(b) == 4) as.numeric(b) else rep(NA_real_, 4)
  })
  bbox <- if (n > 0) do.call(rbind, bbox) else matrix(numeric(), 0, 4)
  interval <- lapply(docs, function(x) x$extent$temporal$interval[[1]])
  time_at <- function(i) do.call(c, lapply(interval, function(v) meta_time(v[[i]])))

  out <- tibble::tibble(
    id = vapply(docs, function(x) meta_text(x$id), ""),
    title = vapply(docs, function(x) meta_text(x$title), ""),
    description = vapply(docs, function(x) meta_text(x$description), ""),
    license = vapply(docs, function(x) meta_text(x$license), ""),
    keywords = lapply(field("keywords"), function(k) as.character(unlist(k))),
    xmin = bbox[, 1], ymin = bbox[, 2], xmax = bbox[, 3], ymax = bbox[, 4],
    start = if (n > 0) time_at(1) else as.POSIXct(character(), tz = "UTC"),
    end = if (n > 0) time_at(2) else as.POSIXct(character(), tz = "UTC"),
    doi = vapply(docs, function(x) meta_text(x[["sci:doi"]]), ""),
    citation = vapply(docs, function(x) meta_text(x[["sci:citation"]]), ""),
    publications = lapply(field("sci:publications"), meta_publications),
    providers = lapply(field("providers"), meta_providers),
    extensions = lapply(field("stac_extensions"), function(e) as.character(unlist(e)))
  )

  # the other fields with a prefix (extensions that the API uses): one list-column each
  others <- setdiff(sort(unique(grep(":", unlist(lapply(docs, names)), value = TRUE))), meta_known_fields)
  for (f in others) out[[f]] <- field(f)
  out
}

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
