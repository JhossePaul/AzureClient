#' @title
#' Put Blob
#'
#' @description
#' The Put Blob operation creates a new block, page, or append blob, or updates
#' the content of an existing block blob.
#'
#' @details
#' Updating an existing block blob overwrites any existing metadata on the blob.
#' Partial updates are not supported with Put Blob; the content of the existing
#' blob is overwritten with the content of the new blob. To perform a partial
#' update of the content of a block blob, use the Put Block List operation.
#'
#' Note that you can create an append blob only in version 2015-02-21 and later.
#'
#' A call to a Put Blob to create a page blob or an append blob only initializes
#' the blob. To add content to a page blob, call the Put Page operation. To add
#' content to an append blob, call the Append Block operation.
#'
#' @inheritParams azure_list_blobs
#' @param blob
#' [character(1): required]\cr
#' Blob name
#'
#' @param blob_type
#' [character(1): required]\cr
#' Valid options are BlockBlob, PageBlob and AppendBlob
#'
#' @param blob_content
#' [raw|character|\code{\link[curl]{form_file}}: required]\cr
#' Blob content. For \code{form_file} objects is better to use
#' \code{\link[httr]{upload_file}} for guessing mime type. The content will be
#' passed as the request body.
#'
#' @section URI Parameters:
#' \describe{
#'   \item{timeout}{Optional. The  timeout parameter is expressed in seconds.
#'   For more information, see Setting Timeouts for Blob Service Operations.}
#' }
#'
#' @section Request Headers:
#' \subsection{All Blob Types}{
#'   \describe{
#'     \item{Authorization}{Required. Specifies the authentication scheme,
#'     account name, and signature. For more information, see Authentication for
#'     the Azure Storage Services.}
#'
#'     \item{Date or x-ms-date}{Required. Specifies the Coordinated Universal
#'     Time (UTC) for the request. For more information, see Authentication for
#'     the Azure Storage Services.}
#'
#'     \item{x-ms-version}{Required for all authenticated requests. Specifies
#'     the version of the operation to use for this request. For more
#'     information, see Versioning for the Azure Storage Services.}
#'
#'     \item{Content-Length}{Required. The length of the request. For a page
#'     blob or an append blob, the value of this header must be set to zero, as
#'     Put Blob is used only to initialize the blob. To write content to an
#'     existing page blob, call Put Page. To write content to an append blob,
#'     call Append Block.}
#'
#'     \item{x-ms-blob-type:<BlockBlob|PageBlob|AppendBlob>}{Required. Specifies
#'     the type of blob to create: block blob, page blob, or append blob.
#'     Support for creating an append blob is available only in version
#'     2015-02-21 and later.}
#'
#'     \item{Content-Type}{Optional. The MIME content type of the blob. The
#'     default type is application/octet-stream.}
#'
#'     \item{Content-Encoding}{Optional. Specifies which content encodings have
#'     been applied to the blob. This value is returned to the client when the
#'     Get Blob operation is performed on the blob resource. The client can use
#'     this value when returned to decode the blob content.}
#'
#'     \item{Content-Language}{Optional. Specifies the natural languages used by
#'     this resource.}
#'
#'     \item{Content-MD5}{Optional. An MD5 hash of the blob content. This hash
#'     is used to verify the integrity of the blob during transport. When this
#'     header is specified, the storage service checks the hash that has arrived
#'     with the one that was sent. If the two hashes do not match, the operation
#'     will fail with error code 400 (Bad Request).  When omitted in version
#'     2012-02-12 and later, the Blob service generates an MD5 hash. Results
#'     from Get Blob, Get Blob Properties, and List Blobs include the MD5 hash.}
#'
#'     \item{Cache-Control}{Optional. The Blob service stores this value but
#'     does not use or modify it.}
#'
#'     \item{x-ms-blob-content-type}{Optional. Set the blob’s content type.}
#'
#'     \item{x-ms-blob-content-encoding}{Optional. Set the blob’s content
#'     encoding.}
#'
#'     \item{x-ms-blob-content-language}{Optional. Set the blob's content
#'     language.}
#'
#'     \item{x-ms-blob-content-md5}{Optional. Set the blob MD5 hash.}
#'
#'     \item{x-ms-blob-cache-control}{Optional. Sets the blob's cache control.}
#'
#'     \item{x-ms-meta-name:value}{Optional. Name-value pairs associated with
#'     the blob as metadata. Note that beginning with version 2009-09-19,
#'     metadata names must adhere to the naming rules for C# identifiers.}
#'
#'     \item{x-ms-lease-id:<ID>}{Required if the blob has an active lease. To
#'     perform this operation on a blob with an active lease, specify the valid
#'     lease ID for this header.}
#'
#'     \item{x-ms-blob-content-disposition}{Optional. Sets the blob
#'     Content-Disposition header. Available for versions 2013-08-15 and later.
#'     The Content-Disposition response header field conveys additional
#'     information about how to process the response payload, and also can be
#'     used to attach additional metadata. For example, if set to attachment, it
#'     indicates that the user-agent should not display the response, but
#'     instead show a Save As dialog with a filename other than the blob name
#'     specified.  The response from the Get Blob and Get Blob Properties
#'     operations includes the content-disposition header.}
#'
#'     \item{Origin}{Optional. Specifies the origin from which the request is
#'     issued.  The presence of this header results in cross-origin resource
#'     sharing headers on the response. See CORS Support for the Storage
#'     Services for details.}
#'
#'     \item{x-ms-client-request-id}{Optional. Provides a client-generated,
#'     opaque value with a 1 KB character limit that is recorded in the
#'     analytics logs when storage analytics logging is enabled. Using this
#'     header is highly recommended for correlating client-side activities with
#'     requests received by the server. For more information, see About Storage
#'     Analytics Logging and Azure Logging: Using Logs to Track Storage
#'     Requests.  This operation also supports the use of conditional headers to
#'     write the blob only if a specified condition is met. For more
#'     information, see Specifying Conditional Headers for Blob Service
#'     Operations.}
#'   }
#' }
#'
#' \subsection{Page Blobs Only}{
#'   \describe{
#'     \item{x-ms-blob-content-length: bytes}{Required for page blobs. This
#'     header specifies the maximum size for the page blob, up to 8 TB. The page
#'     blob size must be aligned to a 512-byte boundary. If this header is
#'     specified for a block blob or an append blob, the Blob service returns
#'     status code 400 (Bad Request).}
#'
#'     \item{x-ms-blob-sequence-number: <num>}{Optional. Set for page blobs
#'     only. The sequence number is a user-controlled value that you can use to
#'     track requests.  The value of the sequence number must be between 0 and
#'     2^63 - 1.The default value is 0.}
#'
#'     \item{x-ms-access-tier}{Version 2017-04-17 and newer. For page blobs on a
#'     premium storage account only. Specifies the tier to be set on the blob.
#'     Check High-performance Premium Storage and managed disks for VMs for a
#'     full list of supported tiers.}
#'   }
#' }
#'
#' @section Authorization:
#' This operation can be called by the account owner and by any client with a
#' shared access signature that has permission to write to this blob or its
#' container.
#'
#' @section Remarks:
#' When you create a blob, you must specify whether it is a block blob, append
#' blob, or page blob by specifying the value of the x-ms-blob-type header. Once
#' a blob has been created, the type of the blob cannot be changed unless it is
#' deleted and re-created.
#'
#' The maximum size for a block blob created via Put Blob is 256 MB for version
#' 2016-05-31 and later, and 64 MB for older versions. If your blob is larger
#' than 256 MB for version 2016-05-31 and later, or 64 MB for older versions,
#' you must upload it as a set of blocks. For more information, see the Put
#' Block and Put Block List operations. It's not necessary to also call Put Blob
#' if you upload the blob as a set of blocks.
#'
#' If you attempt to upload a block blob that is larger than 256 MB for version
#' 2016-05-31 and later, and 64 MB for older versions, or a page blob larger
#' than 8 TB, the service returns status code 413 (Request Entity Too Large).
#' The Blob service also returns additional information about the error in the
#' response, including the maximum blob size permitted in bytes.
#'
#' To create a new page blob, first initialize the blob by calling Put Blob and
#' specify its maximum size, up to 8 TB. When creating a page blob, do not
#' include content in the request body. Once the blob has been created, call Put
#' Page to add content to the blob or to modify it.
#'
#' To create a new append blob, call Put Blob to create a blob with a
#' content-length of zero bytes. Once the append blob is created, call Append
#' Block to add content to the end of the blob.
#'
#' If you call Put Blob to overwrite an existing blob with the same name, any
#' snapshots associated with the original blob are retained. To remove
#' associated snapshots, call Delete Blob first, then Put Blob to re-create the
#' blob.
#'
#' A blob has custom properties (set via headers) that you can use to store
#' values associated with standard HTTP headers. These values can subsequently
#' be read by calling Get Blob Properties, or modified by calling Set Blob
#' Properties. The custom property headers and corresponding standard HTTP
#' header are listed in the following list:
#'
#' \itemize{
#'   \item{Content-Type -> x-ms-blob-content-type}
#'   \item{Content-Encoding -> x-ms-blob-content-encoding}
#'   \item{Content-Language -> x-ms-blob-content-language}
#'   \item{Content-MD5 -> x-ms-blob-content-md5}
#'   \item{Cache-Control -> x-ms-blob-cache-control}
#' }
#' The semantics for setting persisting these property values with the blob as
#' follows:
#'
#' \itemize{
#'     \item{If the client specifies a custom property header, as indicated by
#'     the x-ms-blob prefix, this value is stored with the blob.}
#'
#'     \item{If the client specifies a standard HTTP header, but not the custom
#'     property header, the value is stored in the corresponding custom property
#'     associated with the blob, and is returned by a call to Get Blob
#'     Properties. For example, if the client sets the Content-Type header on
#'     the request, that value is stored in the blob's x-ms-blob-content-type
#'     property.}
#'
#'     \item{If the client sets both the standard HTTP header and the
#'     corresponding property header on the same request, the PUT request uses
#'     the value provided for the standard HTTP header, but the value specified
#'     for the custom property header is persisted with the blob and returned by
#'     subsequent GET requests.}
#'
#'     \item{If the blob has an active lease, the client must specify a valid
#'     lease ID on the request in order to overwrite the blob. If the client
#'     does not specify a lease ID, or specifies an invalid lease ID, the Blob
#'     service returns status code 412 (Precondition Failed). If the client
#'     specifies a lease ID but the blob does not have an active lease, the Blob
#'     service also returns status code 412 (Precondition Failed). If the client
#'     specifies a lease ID on a blob that does not yet exist, the Blob service
#'     will return status code 412 (Precondition Failed) for requests made
#'     against version 2013-08-15 and later; for prior versions the Blob service
#'     will return status code 201 (Created).}
#'
#'     \item{If an existing blob with an active lease is overwritten by a Put
#'     Blob operation, the lease persists on the updated blob, until it expires
#'     or is released.}
#' }
#'
#' A Put Blob operation is permitted 10 minutes per MB to complete. If the
#' operation is taking longer than 10 minutes per MB on average, the operation
#' will timeout.
# '
#' @references
#' \url{https://docs.microsoft.com/en-us/rest/api/storageservices/put-blob}
#'
#' @return
#' \code{\link[httr]{response}}
#'
#' @export
azure_put_blob <- function(
  storage_account, storage_key, container, blob, blob_type, blob_content, ...,
  extra_query = new("namedList"), extra_headers = new("namedList")
) {
  # Assertions --------------------------------------------------------------
  azure_assert_call(match.call(), "timeout")

  # Make request ------------------------------------------------------------
  azure_blob_call(
    verb = "PUT",
    storage_account = storage_account, storage_key = storage_key,
    container = container, blob = blob,
    query = extra_query,
    xheaders = c("x-ms-blob-type" = blob_type, extra_headers),
    body_content = blob_content
  )
}


#' @title
#' Get Blob
#'
#' @description
#' The Get Blob operation reads or downloads a blob from the system, including
#' its metadata and properties. You can also call Get Blob to read a snapshot.
#'
#' @inheritParams azure_put_blob
#'
#' @return
#' \code{\link[httr]{response}} object
#'
#' @section URI Parameters:
#' \describe{
#'   \item{snapshot}{Optional. The snapshot parameter is an opaque DateTime
#'   value that, when present, specifies the blob snapshot to retrieve. For more
#'   information on working with blob snapshots, see Creating a Snapshot of a
#'   Blob.}
#'
#'   \item{timeout}{Optional. The timeout parameter is expressed in seconds. For
#'   more information, see Setting Timeouts for Blob Service Operations.}
#' }
#'
#' @section Request Headers:
#' \itemize{
#'   \item{Authorization}{Required. Specifies the authentication scheme, account
#'   name, and signature. For more information, see Authentication for the Azure
#'   Storage Services.}
#'
#'   \item{Date or x-ms-date}{Required. Specifies the Coordinated Universal Time
#'   (UTC) for the request. For more information, see Authentication for the
#'   Azure Storage Services.}
#'
#'   \item{x-ms-version}{Required for all authenticated requests, optional for
#'   anonymous requests. Specifies the version of the operation to use for this
#'   request. For more information, see Versioning for the Azure Storage
#'   Services.}
#'
#'   \item{Range}{Optional. Return only the bytes of the blob in the specified
#'   range.}
#'
#'   \item{x-ms-range}{Optional. Return only the bytes of the blob in the
#'   specified range. If both Range and x-ms-range are specified, the service
#'   uses the value of x-ms-range. If neither are specified, the entire blob
#'   contents are returned. See Specifying the Range Header for Blob Service
#'   Operations for more information.}
#'
#'   \item{x-ms-lease-id:<ID>}{Optional. If this header is specified, the
#'   operation will be performed only if both of the following conditions are
#'   met:
#'     \itemize{
#'       \item{The blob's lease is currently active.}
#'       \item{The lease ID specified in the request matches that of the blob.}
#'     }
#'     If this header is specified and both of these conditions are not met, the
#'     request will fail and the Get Blob operation will fail with status code
#'     412 (Precondition Failed).
#'   }
#'
#'   \item{x-ms-range-get-content-md5: true}{Optional. When this header is set
#'   to true and specified together with the Range header, the service returns
#'   the MD5 hash for the range, as long as the range is less than or equal to 4
#'   MB in size.  If this header is specified without the Range header, the
#'   service returns status code 400 (Bad Request). If this header is set to
#'   true when the range exceeds 4 MB in size, the service returns status code
#'   400 (Bad Request).}
#'
#'   \item{Origin}{Optional. Specifies the origin from which the request is
#'   issued.  The presence of this header results in cross-origin resource
#'   sharing (CORS) headers on the response.}
#'
#'   \item{x-ms-client-request-id}{Optional. Provides a client-generated, opaque
#'   value with a 1 KB character limit that is recorded in the analytics logs
#'   when storage analytics logging is enabled. Using this header is highly
#'   recommended for correlating client-side activities with requests received
#'   by the server. For more information, see About Storage Analytics Logging
#'   and Azure Logging: Using Logs to Track Storage Requests.}
#' }
#' This operation also supports the use of conditional headers to read the blob
#' only if a specified condition is met. For more information, see Specifying
#' Conditional Headers for Blob Service Operations.
#'
#' @section Authorization:
#' If the container's access control list (ACL) is set to allow anonymous access
#' to the blob, any client may call this operation. If the container is private,
#' this operation can be performed by the account owner and by anyone with a
#' Shared Access Signature that has permission to read the blob.
#'
#' @section Remarks:
#' For a page blob, a Get Blob operation over a range of pages that do not yet
#' have content or that have been cleared returns zeros for those bytes.
#'
#' If you call Get Blob on a page blob with no range specified, the service
#' returns the range of pages up to the value specified for the
#' x-ms-blob-content-length header. For any pages lacking content, the service
#' returns zeros for those bytes.
#'
#' For an append blob, the Get Blob operation returns
#' x-ms-blob-committed-block-count header. This header indicates the number of
#' committed blocks in the blob. The x-ms-blob-committed-block-count header is
#' not returned for block blobs or page blobs.
#'
#' A Get Blob operation is allowed 2 minutes per MB to complete. If the
#' operation is taking longer than 2 minutes per MB on average, the operation
#' will time out.
#'
#' The x-ms-version header is required to retrieve a blob that belongs to a
#' private container. If the blob belongs to a container that is available for
#' full or partial public access, any client can read it without specifying a
#' version; the service version is not required for retrieving a blob that
#' belongs to a public container. See Restrict Access to Containers and Blobs
#' for more information.
#'
#' \subsection{Copy operations}{
#'   To determine if a Copy Blob operation has completed, first check that the
#'   x-ms-copy-id header value of the destination blob matches the copy ID
#'   provided by the original call to Copy Blob. A match assures that another
#'   application did not abort the copy and start a new Copy Blob operation.
#'   Then check for the x-ms-copy-status: success header. However, be aware that
#'   all write operations on a blob except Lease, Put Page and Put Block
#'   operations remove all x-ms-copy-* properties from the blob. These
#'   properties are also not copied by Copy Blob operations that use versions
#'   before 2012-02-12.
#'
#'   When x-ms-copy-status: failed appears in the response,
#'   x-ms-copy-status-description contains more information about the Copy Blob
#'   failure.
#' }
#'
#' @references
#' \url{https://docs.microsoft.com/en-us/rest/api/storageservices/get-blob}
#'
#' @export
azure_get_blob <- function(
  storage_account, storage_key, container, blob,
  extra_query = new("namedList"), extra_headers = new("namedList")
) {
  # Assertions --------------------------------------------------------------
  azure_assert_call(match.call(), c("timeout", "snapshot"))

  # Base values -------------------------------------------------------------
  query <- extra_query
  xheaders <- extra_headers

  # Make request ------------------------------------------------------------
  azure_blob_call(
    verb = "GET",
    storage_account = storage_account, storage_key = storage_key,
    container = container, blob = blob,
    query = query, xheaders = xheaders
  )
}

#' @title
#' Get Blob Properties
#'
#' @description
#' The Get Blob Properties operation returns all user-defined metadata, standard
#' HTTP properties, and system properties for the blob. It does not return the
#' content of the blob.
#'
#' @inheritParams azure_put_blob
#'
#' @inheritSection azure_get_blob URI Parameters
#' @inheritSection azure_list_storage_containers Request Headers
#'
#' @section Request Headers:
#' This operation also supports the use of conditional headers to return blob
#' properties and metadata only if a specified condition is met. For more
#' information, see Specifying Conditional Headers for Blob Service Operations.
#'
#' @inheritSection azure_get_blob Authorization
#'
#' @section Remarks:
#' To determine if a Copy Blob operation has completed, first check that the
#' x-ms-copy-id header value of the destination blob matches the copy ID
#' provided by the original call to Copy Blob. A match assures that another
#' application did not abort the copy and start a new Copy Blob operation.
#' Then check for the x-ms-copy-status: success header. However, be aware that
#' all write operations on a blob except Lease, Put Page and Put Block
#' operations remove all x-ms-copy-* properties from the blob. These
#' properties are also not copied by Copy Blob operations that use versions
#' before 2012-02-12.
#'
#' When x-ms-copy-status: failed appears in the response,
#' x-ms-copy-status-description contains more information about the Copy Blob
#' failure.
#'
#' @references
#' \url{https://docs.microsoft.com/en-us/rest/api/storageservices/get-blob-properties}
#'
#' @return
#' \code{\link[httr]{response}} object
#'
#' @export
azure_get_blob_properties <- function(
  storage_account, storage_key, container, blob,
  extra_query = new("namedList"), extra_headers = new("namedList")
) {
  # Assertions --------------------------------------------------------------
  azure_assert_call(match.call(), c("timeout", "snapshot"))

  # Make request ------------------------------------------------------------
  azure_blob_call(
    verb = "HEAD",
    storage_account = storage_account, storage_key = storage_key,
    container = container, blob = blob,
    query = extra_query, xheaders = extra_headers
  )
}

#' Set Blob Properties
#'
#' The Set Blob Properties operation sets system properties on the blob.
#'
#' @inheritParams azure_put_blob
#'
#' @inheritSection azure_put_blob URI Parameters
#' @section Request Headers:
#' \subsection{All Blob Types}{
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
#'   \item{x-ms-blob-cache-control}{Optional. Modifies the cache control string
#'   for the blob. If this property is not specified on the request, then the
#'   property will be cleared for the blob. Subsequent calls to Get Blob
#'   Properties will not return this property, unless it is explicitly set on
#'   the blob again.}
#'
#'   \item{x-ms-blob-content-type}{Optional. Sets the blob’s content type. If
#'   this property is not specified on the request, then the property will be
#'   cleared for the blob. Subsequent calls to Get Blob Properties will not
#'   return this property, unless it is explicitly set on the blob again.}
#'
#'   \item{x-ms-blob-content-md5}{Optional. Sets the blob's MD5 hash. If this
#'   property is not specified on the request, then the property will be cleared
#'   for the blob. Subsequent calls to Get Blob Properties will not return this
#'   property, unless it is explicitly set on the blob again.}
#'
#'   \item{x-ms-blob-content-encoding}{Optional. Sets the blob's content
#'   encoding. If this property is not specified on the request, then the
#'   property will be cleared for the blob. Subsequent calls to Get Blob
#'   Properties will not return this property, unless it is explicitly set on
#'   the blob again.}
#'
#'   \item{x-ms-blob-content-language}{Optional. Sets the blob's content
#'   language. If this property is not specified on the request, then the
#'   property will be cleared for the blob. Subsequent calls to Get Blob
#'   Properties will not return this property, unless it is explicitly set on
#'   the blob again.}
#'
#'   \item{x-ms-lease-id:<ID>}{Required if the blob has an active lease. To
#'   perform this operation on a blob with an active lease, specify the valid
#'   lease ID for this header.}
#'
#'   \item{x-ms-client-request-id}{Optional. Provides a client-generated, opaque
#'   value with a 1 KB character limit that is recorded in the analytics logs
#'   when storage analytics logging is enabled. Using this header is highly
#'   recommended for correlating client-side activities with requests received
#'   by the server. For more information, see About Storage Analytics Logging
#'   and Azure Logging: Using Logs to Track Storage Requests.}
#'
#'   \item{x-ms-blob-content-disposition}{Optional. Sets the blob’s
#'   Content-Disposition header. Available for versions 2013-08-15 and later.
#'   The Content-Disposition response header field conveys additional
#'   information about how to process the response payload, and also can be used
#'   to attach additional metadata. For example, if set to attachment, it
#'   indicates that the user-agent should not display the response, but instead
#'   show a Save As dialog with a filename other than the blob name specified.
#'   The response from the Get Blob and Get Blob Properties operations includes
#'   the content-disposition header.}
#'
#'   \item{Origin}{Optional. Specifies the origin from which the request is
#'   issued. The presence of this header results in cross-origin resource
#'   sharing headers on the response. See CORS Support for the Storage Services
#'   for details.  This operation also supports the use of conditional headers
#'   to set blob properties only if a specified condition is met. For more
#'   information, see Specifying Conditional Headers for Blob Service
#'   Operations.}
#' }}
#'
#' \subsection{Page Blobs Only}{
#' \describe{
#'   \item{x-ms-blob-content-length: byte value}{Optional. Resizes a page blob
#'   to the specified size. If the specified value is less than the current size
#'   of the blob, then all pages above the specified value are cleared. This
#'   property cannot be used to change the size of a block blob or an append
#'   blob. Setting this property for a block blob or an append blob returns
#'   status code 400 (Bad Request).}
#'
#'   \item{x-ms-sequence-number-action:<max, update, increment>}{Optional, but
#'   required if the x-ms-blob-sequence-number header is set for the request.
#'   This property applies to page blobs only. This property indicates how the
#'   service should modify the blob's sequence number. Specify one of the
#'   following options for this property:
#'     \itemize{
#'       \item{max: Sets the sequence number to be the higher of the value
#'       included with the request and the value currently stored for the blob.}
#'
#'       \item{update: Sets the sequence number to the value included with the
#'       request.}
#'
#'       \item{increment: Increments the value of the sequence number by 1. If
#'       specifying this option, do not include the x-ms-blob-sequence-number
#'       header; doing so will return status code 400 (Bad Request).}
#'     }
#'   }
#'
#'   \item{x-ms-blob-sequence-number: <num>}{Optional, but required if the
#'   x-ms-sequence-number-action property is set to max or update. This property
#'   applies to page blobs only. This property sets the blob's sequence number.
#'   The sequence number is a user-controlled property that you can use to track
#'   requests and manage concurrency issues. For more information, see the Put
#'   Page operation. Use this property together with the
#'   x-ms-sequence-number-action to update the blob's sequence number, either to
#'   the specified value or to the higher of the values specified with the
#'   request or currently stored with the blob. This header should not be
#'   specified if x-ms-sequence-number-action is set to increment; in this case
#'   the service automatically increments the sequence number by one. To set the
#'   sequence number to a value of your choosing, this property must be
#'   specified on the request together with x-ms-sequence-number-action.}
#' }}
#'
#' @inheritSection azure_get_blob_properties Authorization
#'
#' @section Remarks:
#' The semantics for updating a blob's properties are as follows:
#' \itemize{
#'   \item{
#'     A page blob's sequence number is updated only if the request meets either
#'      of the following conditions:
#'   }
#'     \itemize{
#'       \item{
#'         The request sets the x-ms-sequence-number-action to max or update,
#'         and also specifies a value for the x-ms-blob-sequence-number header.
#'        }
#'       \item{
#'         The request sets the x-ms-sequence-number-action to increment,
#'         indicating that the service should increment the sequence number by
#'         one.
#'        }
#'     }
#'   \item{
#'     A page blob's size is modified only if the request specifies a value for
#'      the x-ms-content-length header.
#'    }
#'   \item{
#'     To change a page blob's size in a premium storage account, the new size
#'     must not exeed the content length allowed by the existing tier. Call Set
#'     Blob Tier before resizing the blob. For a list of tiers and allowed
#'     content length, see High-performance Premium Storage and managed disks
#'     for VMs.
#'   }
#'   \item{
#'     If a request sets only x-ms-blob-sequence-number and/or
#'     x-ms-content-length, and no other properties, then none of the blob's
#'      other properties are modified.
#'   }
#'   \item{
#'     If any one or more of the following properties is set in the request,
#'     then all of these properties are set together. If a value is not provided
#'     for a given property when at least one of the properties listed below is
#'     set, then that property will be cleared for the blob.
#'   }
#'   \itemize{
#'     \item{x-ms-blob-cache-control}
#'     \item{x-ms-blob-content-type}
#'     \item{x-ms-blob-content-md5}
#'     \item{x-ms-blob-content-encoding}
#'     \item{x-ms-blob-content-language}
#'     \item{x-ms-blob-content-disposition}
#'   }
#' }
#'
#' Note that for a shared access signature, you can override certain properties
#' stored for the blob by specifying query parameters as part of the shared
#' access signature. These properties include the cache-control, content-type,
#' content-encoding, content-language, and content-disposition properties.
#' For more information, see Constructing a Service SAS.
#'
#' @return
#' \code{\link[httr]{response}} object
#'
#' @references
#' \url{https://docs.microsoft.com/en-us/rest/api/storageservices/set-blob-properties}
#'
#' @export
azure_set_blob_properties <- function(
  storage_account, storage_key, container, blob,
  extra_query = new("namedList"), extra_headers = new("namedList"), ...
) {
  # Assertions --------------------------------------------------------------
  azure_assert_call(match.call())

  # Make request ------------------------------------------------------------
  azure_blob_call(
    verb = "PUT",
    storage_account = storage_account, storage_key = storage_key,
    container = container, blob = blob,
    query = c(list(comp = "properties"), extra_query),
    xheaders = extra_headers
  )
}

#' Get Blob Metadata
#'
#' The Get Blob Metadata operation returns all user-defined metadata for the
#' specified blob or snapshot. +
#'
#' @inheritParams azure_put_blob
#'
#' @inheritSection azure_get_blob URI Parameters
#' @inheritSection azure_get_blob Request Headers
#'
#' @section Authorization:
#' This operation can be performed by the account owner or by anyone using a
#' Shared Access Signature that has permission to read the blob. If the
#' container's ACL is set to allow anonymous access, any client may call this
#' operation.
#'
#' @references
#' \url{https://docs.microsoft.com/en-us/rest/api/storageservices/get-blob-metadata}
#'
#' @return
#' \code{\link[httr]{response}} object
#'
#' @export
azure_get_blob_metadata <- function(
  storage_account, storage_key, container, blob,
  extra_query = new("namedList"), extra_headers = new("namedList")
) {
  # Assertions --------------------------------------------------------------
  azure_assert_call(match.call(), c("timeout", "snapshot"))

  # Base values -------------------------------------------------------------

  # Make request ------------------------------------------------------------
  azure_blob_call(
    verb = "GET",
    storage_account = storage_account, storage_key = storage_key,
    container = container, blob = blob,
    query = c(comp = "metadata", extra_query),
    xheaders = extra_headers
  )
}

#' Set Blob Metadata
#'
#' The Set Blob Metadata operation sets user-defined metadata for the specified
#' blob as one or more name-value pairs.
#'
#' @inheritParams azure_get_blob
#'
#' @inheritSection azure_put_blob URI Parameters
#'
#' @inheritSection azure_set_container_metadata Request Headers
#' @section Request Headers:
#' This operation also supports the use of conditional headers to set blob
#' metadata only if a specified condition is met. For more information, see
#' Specifying Conditional Headers for Blob Service Operations.
#' @inheritSection azure_get_blob_metadata Authorization
#'
#' @section Remarks:
#' If the blob has an active lease, the client must specify a valid lease ID on
#' the request in order to write metadata to the blob. If the client does not
#' specify a lease ID, or specifies an invalid lease ID, the Blob service
#' returns status code 412 (Precondition Failed). If the client specifies a
#' lease ID but the blob does not have an active lease, the Blob service also
#' returns status code 412 (Precondition Failed).
#'
#' @references
#' \url{https://docs.microsoft.com/en-us/rest/api/storageservices/set-blob-metadata}
#'
#' @return httr reponse object
#'
#' @export
azure_set_blob_metadata <- function(
  storage_account, storage_key, container, blob,
  extra_query = new("namedList"), extra_headers = new("namedList")
) {
  # Assertions --------------------------------------------------------------
  azure_assert_call(match.call(), c("timeout", "snapshot"))

  # Make request ------------------------------------------------------------
  azure_blob_call(
    verb = "PUT",
    storage_account = storage_account, storage_key = storage_key,
    container = container, blob = blob,
    query = c(comp = "metadata", extra_query),
    xheaders = extra_headers
  )
}


#' @title
#' Snapshot Blob
#'
#' @description
#' The Snapshot Blob operation creates a read-only snapshot of a blob.
#'
#' @inheritParams azure_put_blob
#'
#' @inheritSection azure_put_blob URI Parameters
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
#'   \item{x-ms-meta-name:value}{Optional. Specifies a user-defined name-value
#'   pair associated with the blob. If no name-value pairs are specified, the
#'   operation will copy the base blob metadata to the snapshot. If one or more
#'   name-value pairs are specified, the snapshot is created with the specified
#'   metadata, and metadata is not copied from the base blob. Note that
#'   beginning with version 2009-09-19, metadata names must adhere to the naming
#'   rules for C# identifiers.  See Naming and Referencing Containers, Blobs,
#'   and Metadata for more information.}
#'
#'   \item{If-Modified-Since}{Optional. A DateTime value. Specify this
#'   conditional header to snapshot the blob only if it has been modified since
#'   the specified date/time. If the base blob has not been modified, the Blob
#'   service returns status code 412 (Precondition Failed).}
#'
#'   \item{If-Unmodified-Since}{Optional. A DateTime value. Specify this
#'   conditional header to snapshot the blob only if it has not been modified
#'   since the specified date/time. If the base blob has been modified, the Blob
#'   service returns status code 412 (Precondition Failed).}
#'
#'   \item{If-Match}{Optional. An ETag value. Specify an ETag value for this
#'   conditional header to snapshot the blob only if its ETag value matches the
#'   value specified. If the values do not match, the Blob service returns
#'   status code 412 (Precondition Failed).}
#'
#'   \item{If-None-Match}{Optional. An ETag value. Specify an ETag value for
#'   this conditional header to snapshot the blob only if its ETag value does
#'   not match the value specified. If the values are identical, the Blob
#'   service returns status code 412 (Precondition Failed).}
#'
#'   \item{x-ms-lease-id:<ID>}{Optional. If this header is specified, the
#'   operation will be performed only if both of the following conditions are
#'   met:
#'   \itemize{
#'     \item{The blob's lease is currently active.}
#'     \item{The lease ID specified in the request matches that of the blob.}
#'   }
#'   If this header is specified and both of these conditions are not met, the
#'   request will fail and the Snapshot Blob operation will fail with status
#'   code 412 (Precondition Failed).}
#'
#'   \item{x-ms-client-request-id}{Optional. Provides a client-generated, opaque
#'   value with a 1 KB character limit that is recorded in the analytics logs
#'   when storage analytics logging is enabled. Using this header is highly
#'   recommended for correlating client-side activities with requests received
#'   by the server. For more information, see About Storage Analytics Logging
#'   and Azure Logging: Using Logs to Track Storage Requests.}
#' }
#'
#' @return
#' \code{\link[httr]{response}}
#'
#' @section Authorization:
#' Only the account owner may call this operation.
#'
#' @section Remarks:
#' A snapshot provides a convenient way to back up blob data. You can use a
#' snapshot to restore a blob to an earlier version by calling Copy Blob to
#' overwrite a base blob with its snapshot.
#'
#' When you create a snapshot, the Blob service returns a DateTime value that
#' uniquely identifies the snapshot relative to its base blob. You can use this
#' value to perform further operations on the snapshot. Note that you should
#' treat this DateTime value as opaque.
#'
#' The DateTime value identifies the snapshot on the URI. For example, a base
#' blob and its snapshots have URIs similar to the following:
#'
#' \itemize{
#'   \item{Base blob: http://myaccount.blob.core.windows.net/mycontainer/myblob}
#'   \item{Snapshot: http://myaccount.blob.core.windows.net/mycontainer/myblob?snapshot=<DateTime>}
#' }
#'
#' Note that each time you call the Snapshot Blob operation, a new snapshot is
#' created, with a unique DateTime value. A blob can support any number of
#' snapshots. Existing snapshots are never overwritten, but must be deleted
#' explicitly by calling Delete Blob and setting the x-ms-include-snapshots
#' header to the appropriate value.
#'
#' @section Reading, Copying, and Deleting Snapshots:
#' A successful call to Snapshot Blob returns a DateTime value in the
#' x-ms-snapshot response header. You can then use this DateTime value to
#' perform read, delete, or copy operations on a particular snapshot version.
#' Any Blob service operation that is valid for a snapshot can be called by
#' specifying ?snapshot=<DateTime> after the blob name.
#'
#' @section Copying Blob Properties and Metadata:
#' When you create a snapshot of a blob, the following system properties are
#' copied to the snapshot with the same values:
#'
#' \itemize{
#'   \item{Content-Type}
#'   \item{Content-Encoding}
#'   \item{Content-Language}
#'   \item{Content-Length}
#'   \item{Cache-Control}
#'   \item{Content-MD5}
#'   \item{x-ms-blob-sequence-number (for page blobs only)}
#'   \item{x-ms-blob-committed-block-count (for append blobs only)}
#'   \item{x-ms-copy-id (version 2012-02-12 and newer)}
#'   \item{x-ms-copy-status (version 2012-02-12 and newer)}
#'   \item{x-ms-copy-source (version 2012-02-12 and newer)}
#'   \item{x-ms-copy-progress (version 2012-02-12 and newer)}
#'   \item{x-ms-copy-completion-time (version 2012-02-12 and newer)}
#'   \item{x-ms-copy-status-description (version 2012-02-12 and newer)}
#' }
#'
#' The base blob's committed block list is also copied to the snapshot, if the
#' blob is a block blob. Any uncommitted blocks are not copied.
#'
#' The snapshot blob is always the same size as the base blob at the time the
#' snapshot is taken, so the value of the Content-Length header for the snapshot
#' blob will be the same as that for the base blob.
#'
#' You can specify one or more new metadata values for the snapshot by
#' specifying the x-ms-meta-name:value header on the request. If this header is
#' not specified, the metadata associated with the base blob is copied to the
#' snapshot.
#'
#' @section Specifying Conditional Headers:
#' You can specify conditional headers on the request to snapshot the blob only
#' if a condition is met. If the specified condition is not met, the snapshot is
#' not created, and the Blob service returns status code 412 (Precondition
#' Failed), along with additional error information about the unmet condition.
#'
#' @section Copying Snapshots:
#' When a base blob is copied using the Copy Blob operation, any snapshots of
#' the base blob are not copied to the destination blob. When a destination blob
#' is overwritten with a copy, any snapshots associated with the destination
#' blob stay intact under its name.
#'
#' You can copy a snapshot blob over its base blob to restore an earlier version
#' of a blob. The snapshot remains, but the base blob is overwritten with a copy
#' that can be both read and written.
#'
#' @references
#' \url{https://docs.microsoft.com/en-us/rest/api/storageservices/snapshot-blob}
#'
#' @export
azure_snapshot_blob <- function(
  storage_account, storage_key, container, blob,
  extra_query = new("namedList"), extra_headers = new("namedList")
) {
  # Assertions --------------------------------------------------------------
  azure_assert_call(match.call())

  # Make request ------------------------------------------------------------
  azure_blob_call(
    verb = "PUT",
    storage_account = storage_account, storage_key = storage_key,
    container = container, blob = blob,
    query = c(comp = "snapshot", extra_query),
    xheaders = extra_headers
  )
}

#' Delete Blob
#'
#' The Delete Blob operation marks the specified blob or snapshot for deletion.
#' The blob is later deleted during garbage collection.
#'
#' Note that in order to delete a blob, you must delete all of its snapshots.
#' You can delete both at the same time with the Delete Blob operation.
#'
#' @inheritParams azure_get_blob
#'
#' @return httr response object
#'
#' @inheritSection azure_get_blob URI Parameters
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
#'   \item{x-ms-version}{Required for all authenticated requests. For more
#'   information, see Versioning for the Azure Storage Services.}
#'
#'   \item{x-ms-lease-id:<ID>}{Required if the blob has an active lease. To
#'   perform this operation on a blob with an active lease, specify the valid
#'   lease ID for this header. If a valid lease ID is not specified on the
#'   request, the operation will fail with status code 403 (Forbidden).}
#'
#'   \item{x-ms-delete-snapshots:<include, only>}{Required if the blob has
#'   associated snapshots. Specify one of the following two options:
#'   \itemize{
#'     \item{include: Delete the base blob and all of its snapshots.}
#'     \item{only: Delete only the blob's snapshots and not the blob itself.}
#'   }
#'   This header should be specified only for a request against the base blob
#'   resource. If this header is specified on a request to delete an individual
#'   snapshot, the Blob service returns status code 400 (Bad Request). If this
#'   header is not specified on the request and the blob has associated
#'   snapshots, the Blob service returns status code 409 (Conflict).}
#'
#'   \item{x-ms-client-request-id}{Optional. Provides a client-generated, opaque
#'   value with a 1 KB character limit that is recorded in the analytics logs
#'   when storage analytics logging is enabled. Using this header is highly
#'   recommended for correlating client-side activities with requests received
#'   by the server. For more information, see About Storage Analytics Logging
#'   and Azure Logging: Using Logs to Track Storage Requests.}
#' }
#' This operation also supports the use of conditional headers to delete the
#' blob only if a specified condition is met. For more information, see
#' Specifying Conditional Headers for Blob Service Operations.
#'
#' @section Authorization:
#' This operation can be performed by the account owner or by anyone using a
#' Shared Access Signature that has permission to delete the blob.
#'
#' @section Remarks:
#' When a blob is successfully deleted, it is immediately removed from the
#' storage account's index and is no longer accessible to clients. The blob's
#' data is later removed from the service during garbage collection.  If the
#' blob has an active lease, the client must specify a valid lease ID on the
#' request in order to delete it.  If a blob has a large number of snapshots,
#' it's possible that the Delete Blob operation will time out. If this happens,
#' the client should retry the request.  For version 2013-08-15 and later, the
#' client may call Delete Blob to delete uncommitted blobs. An uncommitted blob
#' is a blob that was created with calls to the Put Block operation but never
#' committed using the Put Block List operation. For earlier versions, the
#' client must commit the blob first before deleting it.
#'
#' @references
#' \url{https://docs.microsoft.com/en-us/rest/api/storageservices/delete-blob}
#'
#' @export
azure_delete_blob <- function(
  storage_account, storage_key, container, blob,
  extra_query = new("namedList"), extra_headers = new("namedList")
) {
  # Assertions --------------------------------------------------------------
  azure_assert_call(match.call(), c("timeout", "snapshot"))

  # Make request ------------------------------------------------------------
  azure_blob_call(
    verb = "DELETE",
    storage_account = storage_account, storage_key = storage_key,
    container = container, blob = blob,
    query = extra_query, xheaders = extra_headers
  )
}
