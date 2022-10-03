#' @title
#' Put Block
#'
#' @description
#' The Put Block operation creates a new block to be committed as part of a
#' blob.
#'
#' @inheritParams azure_put_blob
#'
#' @return
#' \code{\link[httr]{response}} object
#'
#' @inheritSection azure_put_blob URI Parameters
#'
#' @section Authorization:
#' This operation can be called by the account owner and by anyone with a Shared
#' Access Signature that has permission to write to this blob or its container.
#'
#' @section Remarks:
#' Put Block uploads a block for future inclusion in a block blob. A block blob
#' can include a maximum of 50,000 blocks. Each block can be a different size,
#' up to a maximum of 100 MB for version 2016-05-31 and later, and 4 MB for
#' older versions. The maximum size of a block blob is therefore slightly more
#' than 4.75 TB (100 MB X 50,000 blocks) for version 2016-05-31 and later, and
#' 195 GB (4 MB X 50,000 blocks) for all older versions.
#'
#' A blob can have a maximum of 100,000 uncommitted blocks at any given time.
#' Starting in version 2016-05-31, the set of uncommitted blocks cannot exceed
#' 9.52 TB in total size. For older versions, the set of uncommitted blocks
#' cannot exceed 400 GB in total size. If these maximums are exceeded, the
#' service returns status code 409
#' (RequestEntityTooLargeBlockCountExceedsLimit).
#'
#' After you have uploaded a set of blocks, you can create or update the blob on
#' the server from this set by calling the Put Block List operation. Each block
#' in the set is identified by a block ID that is unique within that blob. Block
#' IDs are scoped to a particular blob, so different blobs can have blocks with
#' same IDs.
#'
#' If you call Put Block on a blob that does not yet exist, a new block blob is
#' created with a content length of 0. This blob is enumerated by the List Blobs
#' operation if the include=uncommittedblobs option is specified. The block or
#' blocks that you uploaded are not committed until you call Put Block List on
#' the new blob. A blob created this way is maintained on the server for a week;
#' if you have not added more blocks or committed blocks to the blob within that
#' time period, then the blob is garbage collected.
#'
#' A block that has been successfully uploaded with the Put Block operation does
#' not become part of a blob until it is committed with Put Block List. Before
#' Put Block List is called to commit the new or updated blob, any calls to Get
#' Blob return the blob contents without the inclusion of the uncommitted block.
#'
#' If you upload a block that has the same block ID as another block that has
#' not yet been committed, the last uploaded block with that ID will be
#' committed on the next successful Put Block List operation.
#'
#' After Put Block List is called, all uncommitted blocks specified in the block
#' list are committed as part of the new blob. Any uncommitted blocks that were
#' not specified in the block list for the blob will be garbage collected and
#' removed from the Blob service. Any uncommitted blocks will also be garbage
#' collected if there are no successful calls to Put Block or Put Block List on
#' the same blob within a week following the last successful Put Block
#' operation. If Put Blob is called on the blob, any uncommitted blocks will be
#' garbage collected.
#'
#' If the blob has an active lease, the client must specify a valid lease ID on
#' the request in order to write a block to the blob. If the client does not
#' specify a lease ID, or specifies an invalid lease ID, the Blob service
#' returns status code 412 (Precondition Failed). If the client specifies a
#' lease ID but the blob does not have an active lease, the Blob service also
#' returns status code 412 (Precondition Failed).
#'
#' For a given blob, all block IDs must be the same length. If a block is
#' uploaded with a block ID of a different length than the block IDs for any
#' existing uncommitted blocks, the service returns error response code 400 (Bad
#' Request).
#'
#' If you attempt to upload a block that is larger than 100 MB for version
#' 2016-05-31 and later, and larger than 4MB for older versions, the service
#' returns status code 413 (Request Entity Too Large). The service also returns
#' additional information about the error in the response, including the maximum
#' block size permitted in bytes.
#'
#' Calling Put Block does not update the last modified time of an existing blob.
#'
#' Calling Put Block on a page blob returns an error.
#'
#' @references
#' \url{https://docs.microsoft.com/en-us/rest/api/storageservices/put-block}
#'
#' @export
azure_put_block <- function(
  storage_account, storage_key, container, blob, blob_content, ...,
  extra_query = new("namedList"), extra_headers = new("namedList")
) {
  # Assertions --------------------------------------------------------------
  # azure_assert_call(match.call())

  # Make request ------------------------------------------------------------
  azure_blob_call(
    verb = "PUT",
    storage_account = storage_account, storage_key = storage_key,
    container = container, blob = blob,
    query = c(
      comp = "block",
      blockid = get_content_md5(blob_content),
      extra_query
    ),
    xheaders = extra_headers,
    body_content = blob_content
  )
}

#' @title
#' Get Block List
#'
#' @description
#' The Get Block List operation retrieves the list of blocks that have been
#' uploaded as part of a block blob.
#'
#' @details
#' There are two block lists maintained for a blob:
#' \describe{
#'   \item{Committed Block List}{The list of blocks that have been successfully
#'   committed to a given blob with Put Block List.}
#'
#'   \item{Uncommitted Block List}{The list of blocks that have been uploaded
#'   for a blob using Put Block, but that have not yet been committed. These
#'   blocks are stored in Azure in association with a blob, but do not yet form
#'   part of the blob.}
#' }
#' You can call Get Block List to return the committed block list, the
#' uncommitted block list, or both lists. You can also call this operation to
#' retrieve the committed block list for a snapshot.
#'
 #' @inheritParams azure_put_blob
#'
#' @return
#' \code{\link[httr]{response}} object
#'
#' @section URI Parameters:
#' \describe{
#'   \item{snapshot}{Optional. The snapshot parameter is an opaque DateTime
#'   value that, when present, specifies the blob list to retrieve. For more
#'   information on working with blob snapshots, see Creating a Snapshot of a
#'   Blob.}
#'
#'   \item{blocklisttype}{Specifies whether to return the list of committed
#'   blocks, the list of uncommitted blocks, or both lists together. Valid
#'   values are committed, uncommitted, or all. If you omit this parameter, Get
#'   Block List returns the list of committed blocks.}
#'
#'   \item{timeout}{Optional. The timeout parameter is expressed in seconds. For
#'   more information, see Setting Timeouts for Blob Service Operations.}
#' }
#'
#' @inheritSection azure_list_storage_containers Request Headers
#'
#' @section Authorization:
#' If the container's ACL is set to allow anonymous access, any client may call
#' Get Block List; however, only committed blocks can be accessed publicly.
#' Access to the uncommitted block list is restricted to the account owner and
#' to anyone using a Shared Access Signature that has permission to read this
#' blob or its container.
#'
#' @section Remarks:
#' Call Get Block List to return the list of blocks that have been committed to
#' a block blob, the list of blocks that have not yet been committed, or both
#' lists. Use the blocklisttype parameter to specify which list of blocks to
#' return.
#'
#' The list of committed blocks is returned in the same order that they were
#' committed by the Put Block List operation. No block may appear more than once
#' in the committed block list.
#'
#' You can use the uncommitted block list to determine which blocks are missing
#' from the blob in cases where calls to Put Block or Put Block List have
#' failed. The list of uncommitted blocks is returned in alphabetical order. If
#' a block ID has been uploaded more than once, only the most recently uploaded
#' block appears in the list.
#'
#' Note that when a blob has not yet been committed, calling Get Block List with
#' blocklisttype=all returns the uncommitted blocks, and the CommittedBlocks
#' element is empty.
#'
#' Get Block List does not support concurrency when reading the list of
#' uncommitted blocks. Calls to Get Block List where blocklisttype=uncommitted
#' or blocklisttype=all have a lower maximum request rate than other read
#' operations. For details on target throughput for read operations, see Azure
#' Storage Scalability and Performance Targets.
#'
#' Get Block List applies only to block blobs. Calling Get Block List on a page
#' blob results in status code 400 (Bad Request).
#'
#' @export
azure_get_block_list <- function(
  storage_account, storage_key, container, blob, ...,
  extra_query = new("namedList"), extra_headers = new("namedList")
) {
  # Assertions --------------------------------------------------------------
  azure_assert_call(match.call(), c("timeout", "snapshot", "blocklisttype"))

  # Make request ------------------------------------------------------------
  azure_blob_call(
    verb = "GET",
    storage_account = storage_account, storage_key = storage_key,
    container = container, blob = blob,
    query = c("comp" = "blocklist", extra_query),
    xheaders = extra_headers
  )
}

#' @title
#' Put Block List
#'
#' @description
#' The Put Block List operation writes a blob by specifying the list of block
#' IDs that make up the blob. In order to be written as part of a blob, a block
#' must have been successfully written to the server in a prior Put Block
#' operation.
#'
#' @details
#' You can call Put Block List to update a blob by uploading only those blocks
#' that have changed, then committing the new and existing blocks together. You
#' can do this by specifying whether to commit a block from the committed block
#' list or from the uncommitted block list, or to commit the most recently
#' uploaded version of the block, whichever list it may belong to.
#'
#' @inheritParams azure_put_block
#' @param block_list
#' [character(1): required]\cr
#' A XML file containing the BlockBlob list. This input is obtained from
#' \code{\link{create_put_block_list_body}}. See
#' \href{https://docs.microsoft.com/en-us/rest/api/storageservices/put-block-list}{documentation}
#' for further details.
#'
#' @return
#' \code{\link[httr]{response}} object
#'
#' @inheritSection azure_put_blob URI Parameters
#'
#' @section Request Headers:
#' \subsection{All Blob types}{
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
#'   \item{Content-Length}{Required. The length of the request content in bytes.
#'   Note that this header refers to the content length of the list of blocks,
#'   not of the blob itself.}
#'
#'   \item{Content-MD5}{Optional. An MD5 hash of the request content. This hash
#'   is used to verify the integrity of the request content during transport. If
#'   the two hashes do not match, the operation will fail with error code 400
#'   (Bad Request).  Note that this header is associated with the request
#'   content, and not with the content of the blob itself.}
#'
#'   \item{x-ms-blob-cache-control}{Optional. Sets the blob’s cache control. If
#'   specified, this property is stored with the blob and returned with a read
#'   request. If this property is not specified with the request, then it is
#'   cleared for the blob if the request is successful.}
#'
#'   \item{x-ms-blob-content-type}{Optional. Sets the blob’s content type. If
#'   specified, this property is stored with the blob and returned with a read
#'   request. If the content type is not specified, then it is set to the
#'   default type, which is application/octet-stream.}
#'
#'   \item{x-ms-blob-content-encoding}{Optional. Sets the blob’s content
#'   encoding. If specified, this property is stored with the blob and returned
#'   with a read request. If this property is not specified with the request,
#'   then it is cleared for the blob if the request is successful.}
#'
#'   \item{x-ms-blob-content-language}{Optional. Set the blob’s content
#'   language. If specified, this property is stored with the blob and returned
#'   with a read request. This property is not specified with the request, then
#'   it is cleared for the blob if the request is successful.}
#'
#'   \item{x-ms-blob-content-md5}{Optional. An MD5 hash of the blob content.
#'   Note that this hash is not validated, as the hashes for the individual
#'   blocks were validated when each was uploaded. The Get Blob operation
#'   returns the value of this header in the Content-MD5 response header. If
#'   this property is not specified with the request, then it is cleared for the
#'   blob if the request is successful.}
#'
#'   \item{x-ms-meta-name:value}{Optional. User-defined name-value pairs
#'   associated with the blob. Note that beginning with version 2009-09-19,
#'   metadata names must adhere to the naming rules for C# identifiers.}
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
#'   The Content-Disposition header field conveys additional information about
#'   how to process the response payload, and also can be used to attach
#'   additional metadata. For example, if set to attachment, it indicates that
#'   the user-agent should not display the response, but instead show a Save As
#'   dialog. The response from the Get Blob and Get Blob Properties operations
#'   includes the content-disposition header.}
#' }}
#'
#' @section Request Body:
#' In the request body, you can specify which block list the Blob service should
#' check for the requested block. In this way you can update an existing blob by
#' inserting, replacing, or deleting individual blocks, rather than re-uploading
#' the entire blob. Once you've uploaded the block or blocks that have changed,
#' you can commit a new version of the blob by committing the new blocks
#' together with the existing blocks that you wish to keep.
#'
#' To update a blob, you can specify that the service should look for a block ID
#' in the committed block list, in the uncommitted block list, or in the
#' uncommitted block list first and then in the committed block list. To
#' indicate which approach to use, specify the block ID within the appropriate
#' XML element within the request body, as follows:
#'
#' Specify the block ID within the Committed element to indicate that the Blob
#' service should search only the committed block list for the named block. If
#' the block is not found in the committed block list, it will not be written as
#' part of the blob, and the Blob service will return status code 400 (Bad
#' Request).
#'
#' Specify the block ID within the Uncommitted element to indicate that the Blob
#' service should search only the uncommitted block list for the named block. If
#' the block is not found in the uncommitted block list, it will not be written
#' as part of the blob, and the Blob service will return status code 400 (Bad
#' Request).
#'
#' Specify the block ID within the Latest element to indicate that the Blob
#' service should first search the uncommitted block list. If the block is found
#' in the uncommitted list, that version of the block is the latest and should
#' be written to the blob. If the block is not found in the uncommitted list,
#' then the service should search the committed block list for the named block
#' and write that block to the blob if it is found.
#'
#' The request body for this version of Put Block List uses following XML
#' format:
#' \preformatted{
#' <?xml version="1.0" encoding="utf-8"?>
#' <BlockList>
#' <Committed>first-base64-encoded-block-id</Committed>
#' <Uncommitted>second-base64-encoded-block-id</Uncommitted>
#' <Latest>third-base64-encoded-block-id</Latest>
#' ...
#' </BlockList>}

#' @section Authorization:
#' This operation can be called by the account owner and by anyone with a Shared
#' Access Signature that has permission to write to this blob or its container.
#'
#' @section Remarks:
#' The Put Block List operation enforces the order in which blocks are to be
#' combined to create a blob.
#'
#' The same block ID can be specified more than one time in the list of blocks.
#' If a block ID is specified more than one time, it will represent the range of
#' bytes in each of those locations in the block list for the final committed
#' blob. If a block ID appears more than once in the list, both instances of the
#' block ID must be specified within the same block list. In other words, both
#' instances must be specified within the Committed element, the Uncommitted
#' element, or the Latest element.
#'
#' With Put Block List, you can modify an existing blob by inserting, updating,
#' or deleting individual blocks, without uploading the whole blob again. You
#' can specify block IDs from both the current committed block list and the
#' uncommitted block list to create a new blob or update the content of an
#' existing blob. In this way you can update a blob by specifying a few new
#' blocks from the uncommitted block list, and the rest from the committed block
#' list, which are already part of the existing blob.
#'
#' If a block ID is specified in the Latest element, and the same block ID
#' exists in both the committed and uncommitted block lists, Put Block List
#' commits the block from the uncommitted block list. If the block ID exists in
#' the committed block list but not in the uncommitted block list, then Put
#' Block List commits the block from the committed block list.
#'
#' Each block can be a different size, up to a maximum of 100 MB for version
#' 2016-05-31 and later, and 4 MB for older versions. The maximum size of a
#' block blob is therefore slightly more than 4.75 TB (100 MB X 50,000 blocks)
#' for version 2016-05-31 and later, and 195 GB (4 MB X 50,000 blocks) for all
#' older versions. If you attempt to commit more than 50,000 blocks, the service
#' returns status code 409 (Block List Too Long). The service also returns
#' additional information about the error in the response, including the maximum
#' number of blocks permitted.
#'
#' The maximum number of uncommitted blocks that may be associated with a blob
#' is 100,000, and the maximum size of the uncommitted block list is about 9.5
#' TB for version 2016-05-31 and later, and 400 GB for older versions. 2
#'
#' When you call Put Block List to update an existing blob, the blob's existing
#' properties and metadata are overwritten. However, any existing snapshots are
#' retained with the blob. You can use the conditional request headers to
#' perform the operation only if a specified condition is met.
#'
#' If the Put Block List operation fails due to a missing block, you will need
#' to upload the missing block.
#'
#' Any uncommitted blocks will be garbage collected if there are no successful
#' calls to Put Block or Put Block List on the blob within a week following the
#' last successful Put Block operation. If Put Blob is called on the blob, any
#' uncommitted blocks will be garbage collected.
#'
#' If the blob has an active lease, the client must specify a valid lease ID on
#' the request in order to commit the block list. If the client does not specify
#' a lease ID, or specifies an invalid lease ID, the Blob service returns status
#' code 412 (Precondition Failed). If the client specifies a lease ID but the
#' blob does not have an active lease, the Blob service also returns status code
#' 412 (Precondition Failed). If the client specifies a lease ID on a blob that
#' does not yet exist, the Blob service will return status code 412
#' (Precondition Failed) for requests made against version 2013-08-15 and later;
#' for prior versions the Blob service will return status code 201 (Created).
#'
#' If the blob has an active lease and you call Put Block List to update the
#' blob, the lease is maintained on the updated blob.
#'
#' Put Block List applies only to block blobs. Calling Put Block List on a page
#' blob results in status code 400 (Bad Request).
#'
#' @export
azure_put_block_list <- function(
  storage_account, storage_key, container, blob, block_list, ...,
  extra_query = new("namedList"), extra_headers = new("namedList")
) {
  # Assertions --------------------------------------------------------------
  azure_assert_call(match.call())

  # Make request ------------------------------------------------------------
  azure_blob_call(
    verb = "PUT",
    storage_account = storage_account, storage_key = storage_key,
    container = container, blob = blob,
    query = c("comp" = "blocklist", extra_query),
    xheaders = extra_headers, body_content = block_list
  )
}
