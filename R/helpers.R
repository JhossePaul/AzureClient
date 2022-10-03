`n'est pas` <- magrittr::`n'est pas`

#' Get x-ms-date header for HTTPS requests
#'
#' @return character length one with a HTTP formated date
#' @examples
#' x_ms_date <- get_x_ms_date()
#'
#' @importFrom httr http_date
#'
#' @export
get_x_ms_date <- function() {
  httr::http_date(Sys.time())
}

#' Get object content length
#'
#' Used to get the "Content-Length" header in HTTP requests.
#'
#' @param obj raw or character length one vector to send in the body
#'
#' @return numeric. Object size in related units
#'
#' @export
get_content_length <- function(obj) {
  content_length <- switch(
    EXPR = class(obj),
    "raw" = length(obj),
    "character" = nchar(obj),
    "form_file" = file.size(obj$path)
  )

  ifelse(content_length == 0L, NULL, content_length)
}

#' Get object content type
#'
#' Used to get the "Content-Type" header in HTTP requests.
#'
#' @param obj raw or character length one vector to send in the body
#'
#' @return character. Object type
#'
#' @export
get_content_type <- function(obj) {
  switch(
    EXPR = class(obj),
    "form_file" = obj$type,
    "character" = "text/plain",
    "raw" = "application/octet-stream"
  )
}

#' Create Azure Storage Service base url
#'
#' @param storage_account (required) character. Azure Storage Account.
#' @param container       (optional) character. Name of the container.
#' @param blob            (optional) character. Name of the Blob.
#'
#' @return character vector length one with the Azure Storage endpoint url.
#'
#' @examples
#' get_azure_storage_url("test")
#' get_azure_storage_url("test", "test")
#' get_azure_storage_url("test", "test", "test")
#'
#' @importFrom checkmate assert_character assert check_character check_null
#' @importFrom magrittr "%>%"
#' @importFrom stringr str_replace_all
#'
#' @export
get_azure_storage_url <- function(
  storage_account = NULL, container = NULL, blob = NULL
) {
  # Assertions --------------------------------------------------------------
  assert_character(storage_account)
  assert(check_character(container), check_null(container))
  assert(check_character(blob), check_null(blob))

  sprintf("https://%s.blob.core.windows.net", storage_account) %>%
    c(container, blob) %>%
    paste(collapse = "/")
}

#' Get the MD5 checksum for a request body
#'
#' @param obj
#' [character|raw|\code{\link[curl]{form_file}}: required]\cr
#' An object to pass to request body
#'
#' @return
#' [character(1)]\cr
#' A character string with the MD5 checksum Base64 encoded
#'
#' @importFrom digest digest
#' @importFrom base64enc base64encode
#'
#' @export
get_content_md5 <- function(obj) {
  switch(
    class(obj),
    "form_file" = digest(obj$path, algo = "md5", raw = TRUE) %>% base64encode,
    digest(obj, algo = "md5", raw = TRUE) %>% base64encode
  )
}
