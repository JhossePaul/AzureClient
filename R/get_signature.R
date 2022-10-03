#' Get the Signature Shared Key for HTTPS request to Azure Blob services
#'
#' In order to authorize the transaction to an Azure Storage Service you need to
#' create a Signature Shared Key and send it through a specialized header. This
#' function creates the Signature Shared Key as a character.
#'
#' @param verb
#' [character(1): <GET|PUT|HEAD|POST|DELETE> required]\cr
#' HTTP verb used in the request
#'
#' @param storage_account
#' [character(1): required]\cr
#' Azure account
#'
#' @param storage_key
#' [character(1): required]\cr
#' Azure Storage Key
#'
#' @param container
#' [character(1): optional]\cr
#' Container name
#'
#' @param blob
#' [character(1): optional]\cr
#' Blob name
#'
#' @param query
#' [named list(character(1)): optional]\cr
#' Extra URI parameters
#'
#' @param xheaders
#' [named list(character(1)): optional]\cr
#' Extra headers to pass in the request
#'
#' @param verbose
#' [logical(1)]\cr
#' Print string to sign?
#'
#' @return
#' [character(1)] Signature Shared Key to authorize an HTTPS request.
#'
#' @references
#' \url{https://docs.microsoft.com/en-us/rest/api/storageservices/authentication-for-the-azure-storage-services}
#'
#' @importFrom base64enc base64decode base64encode
#' @importFrom magrittr extract "%>%"
#' @importFrom stringr str_detect str_replace str_replace_all
#' @importFrom digest hmac
#' @importFrom methods new
#'
#' @export
get_signature <- function(
  verb, storage_account, storage_key,
  container = NULL, blob = NULL,
  query = new("namedList"), xheaders = new("namedList"),
  verbose = FALSE
) {
  # Valid options -----------------------------------------------------------
  valid_headers <- c(
    "Content-Encoding", "Content-Language", "Content-Length", "Content-MD5",
    "Content-Type", "Date", "If-Modified-Since", "If-Match",
    "If-None-Match", "If-Unmodified-Since", "Range"
  )

  # Split headers ----------------------------------------------------------
  hheaders <- xheaders %>%
    extract(names(.) %>% str_detect("x-ms-") %>% `n'est pas`)
  msheaders <- xheaders %>%
    extract(names(.) %>% str_detect("x-ms-"))

  # Content-Length fallback
  hheaders[["Content-Length"]] <- ifelse(
    hheaders[["Content-Length"]] == 0L,
    NULL,
    hheaders[["Content-Length"]]
  )

  # Canonicalize common headers ----------------------------------------------
  canonicalized_hheaders <- hheaders %>%
    unlist %>%
    extract(valid_headers) %>%
    replace(is.na(.), "") %>%
    paste(collapse = "\n")

  # Canonicalize Microsoft headers  -------------------------------------------
  canonicalized_msheaders <- msheaders %>%
    extract(names(.) %>% order) %>%
    paste(names(.), ., sep = ":", collapse = "\n")

  # Canonicalize Resource ----------------------------------------------------
  canonicalized_path <- paste0("/", storage_account, "/") %>%
    c(container, blob) %>%
    paste(collapse = "/") %>%
    str_replace("//", "/")

  canonicalized_query <- query %>%
    extract(names(.) %>% order) %>%
    paste(names(.), ., sep = ":", collapse = "\n")

  # Signature ---------------------------------------------------------------
  str_to_sig <- paste(
    verb,
    canonicalized_hheaders, canonicalized_msheaders,
    canonicalized_path, canonicalized_query,
    sep = "\n"
  ) %>% str_replace("\n+$", "") %>%
    iconv(from = "ASCII", to = "UTF-8")

  # Return encoded signature ------------------------------------------------
  if (verbose) cat(paste0("STRINGTOSIGN: \n", str_to_sig, "\n"))

  base_shared_key <- "SharedKey %s:%s"
  xsignature <- base64decode(storage_key) %>%
    hmac(object = str_to_sig, algo = "sha256", raw = TRUE) %>%
    base64encode

  sprintf(base_shared_key, storage_account, xsignature)
}
