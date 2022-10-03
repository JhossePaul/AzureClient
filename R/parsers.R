#' Parse List Containers content
#'
#' This helper function parses the response from the
#' \code{\link{azure_list_storage_containers}} response.
#'
#' @param list_container_response [\code{\link[httr]{response}}]\cr
#' Response from a \code{\link{azure_list_storage_containers}} call.
#'
#' @return
#' [data.frame(name, last_modified, status, state, etag)] Available containers
#'
#'
#' @importFrom magrittr equals
#' @importFrom httr content
#' @importFrom xml2 xml_find_all xml_text
#'
#' @export
parse_list_containers <- function (list_container_response) {
  containers <- list_container_response %>%
    content(encoding = "UTF-8") %>%
    xml_find_all("//Containers//Container/Name") %>%
    xml_text()

  containers_properties <- list_container_response %>%
    content(encoding = "UTF-8") %>%
    xml_find_all("//Containers//Container/Properties")

  if (containers %>% length %>% equals(0L)) {
    message("No containers found in Storage account")
  }

  data.frame(
    name          = containers,
    last_modified = xml_find_all(containers_properties, "//Last-Modified") %>%
      xml_text,
    status        = xml_find_all(containers_properties, "//LeaseStatus") %>%
      xml_text,
    state         = xml_find_all(containers_properties, "//LeaseState") %>%
      xml_text,
    etag          = xml_find_all(containers_properties, "//Etag") %>%
      xml_text,
    stringsAsFactors = FALSE
  )
}

#' Parse Get BlockList content
#'
#' This helper function parses the response from the
#' \code{\link{azure_get_block_list}} response.
#'
#' @param blocklist_response [\code{\link[httr]{response}}]\cr
#' Response from a \code{\link{azure_get_block_list}} call.
#'
#' @return
#' [data.frame(name, size, status)] Available list in blob
#'
#' @importFrom xml2 xml_find_all xml_text
#' @export
parse_blocklist <- function(blocklist_response) {
  xml_uncommitted <- blocklist_response %>%
    content(encoding = "UTF-8") %>%
    xml_find_all("//BlockList//UncommittedBlocks//Block")
  xml_committed <- blocklist_response %>%
    content(encoding = "UTF-8") %>%
    xml_find_all("//BlockList//CommittedBlocks//Block")

  uncommitted_blocks <- if (length(xml_uncommitted) != 0L) {
    data.frame(
      stringsAsFactors = FALSE,
      name = xml_uncommitted %>%
        xml_find_all("//Name") %>%
        xml_text,
      size = xml_uncommitted %>%
        xml_find_all("//Size") %>%
        xml_text,
      status = "uncommitted"
    )
  } else NULL

  committed_blocks <- if (length(xml_committed) != 0L) {
    data.frame(
      stringsAsFactors = FALSE,
      name = xml_committed %>%
        xml_find_all("//Name") %>%
        xml_text,
      size = xml_committed %>%
        xml_find_all("//Size") %>%
        xml_text,
      status = "committed"
    )
  } else NULL

  rbind(uncommitted_blocks, committed_blocks)
}

#' Create Put Block List body content
#'
#' This helper function creates the request body content for the
#' \code{\link{azure_put_block_list}} from a data.frame listing the
#' blocks to commit.
#'
#' @param blocklist_dataframe [data.frame(name, size, status): required]\cr
#' A data.frame with name, size and status of the blocks to commit to the blob.
#' It would be convenient to use the \code{\link{parse_blocklist}} function.
#'
#' @return
#' [character] An XML file with the format to commit blocks in the
#' \code{\link{azure_put_block_list}} function.
#'
#' @importFrom xml2 xml_new_root xml_add_child xml_set_text
#' @importFrom stringr str_to_title
#'
#' @export
create_put_block_list_body <- function(blocklist_dataframe) {
  root <- xml_new_root("BlockList")

  apply(blocklist_dataframe, 1L, function(row) {
    child <- xml_add_child(root, str_to_title(row["status"]))
    child %>% xml_set_text(row["name"])
  })

  root %>% as.character %>% return
}
