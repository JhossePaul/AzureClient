#' @title
#' List Containers
#'
#' @description
#' The List Containers operation returns a list of the containers under the
#' specified account.
#'
#' @param storage_account
#' [character: required].\cr
#' Azure Storage account
#'
#' @param storage_key
#' [character: required].\cr
#' Azure Storage key
#'
#' @param extra_query
#' [named list: optional (list())].\cr
#' Extra query parameters. See URI Parameters section for further information.
#' \pkg{AzureClient} always takes care of the required URI Parameters.
#'
#' @param extra_headers
#' [named list: optional (list())]\cr
#' Extra HTTP headers. See Request Headers section for more information.
#' \pkg{AzureClient} always takes care of the required Request Headers.
#'
#' @param ...
#' Further arguments passed down to \code{\link[httr]{VERB}}
#'
#' @return
#' \code{\link[httr]{response}} object
#'
#' @family account
#'
#' @section URI Parameters:
#' \describe{
#'   \item{prefix}{[Optional] Filters the results to return only containers
#'   whose name begins with the specified prefix.}
#'
#'   \item{marker}{[Optional] A string value that identifies the portion of the
#'   list of containers to be returned with the next listing operation. The
#'   operation returns the NextMarker value within the response body if the
#'   listing operation did not return all containers remaining to be listed with
#'   the current page. The NextMarker value can be used as the value for the
#'   marker parameter in a subsequent call to request the next page of list
#'   items. Themarker value is opaque to the client.}
#'
#'   \item{maxresults}{Optional. Specifies the maximum number of containers to
#'   return. If the request does not specify maxresults, or specifies a value
#'   greater than 5000, the server will return up to 5000 items. Note that if
#'   the listing operation crosses a partition boundary, then the service will
#'   return a continuation token for retrieving the remainder of the results.
#'   For this reason, it is possible that the service will return fewer results
#'   than specified by maxresults, or than the default of 5000. If the parameter
#'   is set to a value less than or equal to zero, the server returns status
#'   code 400 (Bad Request).}
#'
#'   \item{include=metadata}{Optional. Include this parameter to specify that
#'   the container's metadata be returned as part of the response body. Note
#'   that metadata requested with this parameter must be stored in accordance
#'   with the naming restrictions imposed by the 2009-09-19 version of the Blob
#'   service. Beginning with this version, all metadata names must adhere to the
#'   naming conventions for C# identifiers.}
#'
#'   \item{timeout}{Optional. The timeout parameter is expressed in seconds. For
#'   more information, see Setting Timeouts for Blob Service Operations. }
#' }
#'
#' @section Request Headers:
#' \describe{
#'   \item{Authorization}{Required. Specifies the authentication scheme, account
#'   name, and signature. For more information, see Authentication for the Azure
#'   Storage Services.}
#'
#'   \item{Date or x-ms-date}{Required. Specifies the Coordinated Universal Time
#'   (UTC) for the request. For more information, see Authentication for the
#'   Azure Storage Services.}
#'
#'   \item{x-ms-version}{Required for all authenticated requests. Specifies the
#'   version of the operation to use for this request. For more information, see
#'   Versioning for the Azure Storage Services.}
#'
#'   \item{x-ms-client-request-id}{Optional. Provides a client-generated, opaque
#'   value with a 1 KB character limit that is recorded in the analytics logs
#'   when storage analytics logging is enabled. Using this header is highly
#'   recommended for correlating client-side activities with requests received
#'   by the server. For more information, see About Storage Analytics Logging
#'   and Azure Logging: Using Logs to Track Storage Requests.}
#' }
#'
#' @section Authorization:
#' Only the account owner may call this operation.
#'
#' @section Remarks:
#' If you specify a value for the maxresults parameter and the number of
#' containers to return exceeds this value, or exceeds the default value for
#' maxresults, the response body will contain the NextMarker element (also
#' referred to as a continuation token). NextMarker indicates the next container
#' to return on a subsequent request. To return the next set of items, specify
#' the value of NextMarker for the marker parameter on the URI for the
#' subsequent request. Note that the value of NextMarker should be treated as
#' opaque.
#'
#' If the listing operation crosses a partition boundary, then the service will
#' return a value for the NextMarker element for retrieving the remainder of the
#' results from the next partition. A listing operation that spans more than one
#' partition results in a smaller set of items being returned than is specified
#' by maxresults, or than the default of 5000. Your application should always
#' check for the presence of the NextMarker element when you perform a listing
#' operation, and handle it accordingly.
#'
#' Containers are listed in alphabetical order in the response body.
#'
#' The List Containers operation times out after 30 seconds.
#'
#' @references
#' \url{
#' https://docs.microsoft.com/en-us/rest/api/storageservices/list-containers2
#' }
#'
#' @importFrom checkmate assert assert_character assert_list check_names
#'
#' @export
azure_list_storage_containers <- function(
  storage_account, storage_key, ...,
  extra_query = list(), extra_headers = list()
) {
  # Valid options
  valid_query_params <- c(
    "prefix", "marker", "maxresults",
    "include", "timeout"
  )

  # Assertions --------------------------------------------------------------
  azure_assert_call(match.call(), valid_query_params)

  # Make request ------------------------------------------------------------
  azure_blob_call(
    verb = "GET",
    storage_account = storage_account, storage_key = storage_key,
    query = c(list(comp = "list"), extra_query),
    xheaders = extra_headers
  )
}

#' Get Blob Service properties
#'
#' The Get Blob Service Properties operation gets the properties of a storage
#' account’s Blob service, including properties for Storage Analytics and CORS
#' (Cross-Origin Resource Sharing) rules.
#'
#' For detailed information about CORS rules and evaluation logic, see CORS
#' Support for the Storage Services.
#'
#' @inheritParams azure_list_storage_containers
#'
#' @section URI Parameters:
#' \describe{
#'   \item{restype=service&comp=properties}{[Required] The combination of both
#'    query strings is required to get the storage service properties.}
#'
#'   \item{timeout}{[Optional] The timeout parameter is expressed in seconds.
#'    For more information, see Setting Timeouts for Blob Service Operations.}
#' }
#'
#' @inheritSection azure_list_storage_containers Request Headers
#'
#' @section Authorization:
#' Only the storage account owner may call this operation.
#'
#' @references
#' \url{https://docs.microsoft.com/en-us/rest/api/storageservices/get-blob-service-properties}
#'
#' @return
#' \code{\link[httr]{response}} object
#'
#' @family account
#'
#' @export
azure_get_blob_service_properties <- function(
  storage_account, storage_key, ...,
  extra_query = list(), extra_headers = list()
) {
  # Assertions --------------------------------------------------------------
  azure_assert_call(match.call(), "timeout")

  # Make request ------------------------------------------------------------
  azure_blob_call(
    verb = "GET",
    storage_account = storage_account, storage_key = storage_key,
    query = c(list(restype = "service", comp = "properties"), extra_query),
    xheaders = extra_headers
  )
}

#' Set Blob service properties
#'
#' The Set Blob Service Properties operation sets properties for a storage
#' account’s Blob service endpoint, including properties for Storage Analytics
#' and CORS (Cross-Origin Resource Sharing) rules. See References section.
#'
#' You can also use this operation to set the default request version for all
#' incoming requests to the Blob service that do not have a version specified.
#'
#' @inheritParams azure_list_storage_containers
#' @param properties [raw|character(0)]\cr
#' XML configuration file.
#' See \href{https://docs.microsoft.com/en-us/rest/api/storageservices/set-blob-service-properties#request}{Request body section} for further information.
#'
#' @return
#' \code{\link[httr]{response}} object
#'
#' @family account
#'
#' @inheritSection azure_get_blob_service_properties URI Parameters
#' @inheritSection azure_get_blob_service_properties Request Headers
#'
#' @references
#' \url{https://docs.microsoft.com/en-us/rest/api/storageservices/set-blob-service-properties}
#' \url{https://docs.microsoft.com/en-us/rest/api/storageservices/storage-analytics}
#' \url{https://docs.microsoft.com/en-us/rest/api/storageservices/cross-origin-resource-sharing--cors--support-for-the-azure-storage-services}
#'
#' @export
azure_set_blob_service_properties <- function(
  storage_account, storage_key, properties, ...,
  extra_query = list(), extra_headers = list()
) {
  # Assertions --------------------------------------------------------------
  azure_assert_call(match.call())

  # Make request ------------------------------------------------------------
  azure_blob_call(
    verb = "PUT",
    storage_account = storage_account, storage_key = storage_key,
    query = c(restype = "service", comp = "properties", extra_query),
    xheaders = extra_headers,
    body_content = properties
  )
}

#' Get Azure Blob Service stats
#'
#' The Get Blob Service Stats operation retrieves statistics related to
#' replication for the Blob service. It is only available on the secondary
#' location endpoint when read-access geo-redundant replication is enabled for
#' the storage account.
#'
#' @inheritParams azure_list_storage_containers
#'
#' @return
#' \code{\link[httr]{response}} object
#'
#' @family account
#'
#' @section URI Parameters:
#' \describe{
#'   \item{timeout}{Optional. The timeout parameter is expressed in seconds.}
#' }
#' @inheritSection azure_list_storage_containers Request Headers
#'
#' @references
#' \url{https://docs.microsoft.com/en-us/rest/api/storageservices/get-blob-service-stats}
#'
#' @export
azure_get_blob_service_stats <- function(
  storage_account, storage_key, ...,
  extra_query = list(), extra_headers = list()
) {
  # Assertions --------------------------------------------------------------
  azure_assert_call(match.call())

  # Make request ------------------------------------------------------------
  URL <- paste0(storage_account, "-secondary") %>% get_azure_storage_url
  azure_blob_call(
    verb = "GET", URL = URL,
    storage_account = storage_account, storage_key = storage_key,
    query = c(list(restype = "service", comp = "stats"), extra_query),
    xheaders = extra_headers
  )
}
