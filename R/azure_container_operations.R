#' @title
#' Create an Azure Storage Container
#'
#' @description
#' The Create Container operation creates a new container under the specified
#' account. If the container with the same name already exists, the operation
#' fails.
#'
#' @details
#' The container resource includes metadata and properties for that container.
#' It does not include a list of the blobs contained by the container.
#'
#' @inheritParams azure_list_storage_containers
#' @param container
#' [character(1): required]\cr
#' Name of the container
#'
#' @return
#' \code{\link[httr]{response}} object
#'
#' @section URI Parameters:
#' \describe{
#'   \item{timeout}{Optional. The timeout parameter is expressed in seconds. For
#'   more information, see Setting Timeouts for Blob Service Operations.}
#' }
#' @section Request Headers:
#' \describe{
#'     \item{Authorization}{Required. Specifies the authentication scheme,
#'     account name, and signature. For more information, see Authentication for
#'     the Azure Storage Services.}
#'
#'     \item{Date or x-ms-date}{Required. Specifies the Coordinated Universal
#'     Time (UTC) time for the request. For more information, see Authentication
#'     for the Azure Storage Services.}
#'
#'     \item{x-ms-version}{Required for all authenticated requests. Specifies
#'     the version of the operation to use for this request. For more
#'     information, see Versioning for the Azure Storage Services.}
#'
#'     \item{x-ms-meta-name:value}{Optional. A name-value pair to associate with
#'     the container as metadata. Note that beginning with version 2009-09-19,
#'     metadata names must adhere to the naming rules for C# identifiers.}
#'
#'     \item{x-ms-blob-public-access}{Optional. Specifies whether data in the
#'     container may be accessed publicly and the level of access. Possible
#'     values include:
#'       \itemize{
#'         \item{container: Specifies full public read access for container and
#'         blob data. Clients can enumerate blobs within the container via
#'         anonymous request, but cannot enumerate containers within the storage
#'         account.}
#'
#'         \item{blob: Specifies public read access for blobs. Blob data within
#'         this container can be read via anonymous request, but container data
#'         is not available.  Clients cannot enumerate blobs within the
#'         container via anonymous request.}
#'       }
#'       If this header is not included in the request, container data is
#'       private to the account owner.
#'     }
#'
#'     \item{x-ms-client-request-id}{Optional. Provides a client-generated,
#'     opaque value with a 1 KB character limit that is recorded in the
#'     analytics logs when storage analytics logging is enabled. Using this
#'     header is highly recommended for correlating client-side activities with
#'     requests received by the server. For more information, see About Storage
#'     Analytics Logging and Azure Logging: Using Logs to Track Storage
#'     Requests.}
#' }
#'
#' @section Authorization:
#' Only the account owner may call this operation.
#'
#' @section Remarks:
#' Containers are created immediately beneath the storage account. It's not
#' possible to nest one container beneath another.
#'
#' You can optionally create a default or root container for your storage
#' account. The root container may be inferred from a URL requesting a blob
#' resource. The root container makes it possible to reference a blob from the
#' top level of the storage account hierarchy, without referencing the container
#' name.
#'
#' To add the root container to your storage account, create a container named
#' $root. Construct the request as follows:
#' \code{
#' PUT https://myaccount.blob.core.windows.net/$root?restype=container HTTP/1.1
#' }
#'
#' You can specify metadata for a container at the time it is created by
#' including one or more metadata headers on the request. The format for the
#' metadata header is x-ms-meta-name:value.
#'
#' If a container by the same name is being deleted when Create Container is
#' called, the server will return status code 409 (Conflict), with additional
#' error information indicating that the container is being deleted.
#'
#' @references
#' \url{https://docs.microsoft.com/en-us/rest/api/storageservices/create-container}
#'
#' @export
azure_create_storage_container <- function(
  storage_account, storage_key, container, ...,
  extra_query = new("namedList"), extra_headers = new("namedList")
) {
  # Assertions --------------------------------------------------------------
  azure_assert_call(match.call())

  # Make request ------------------------------------------------------------
  azure_blob_call(
    verb = "PUT",
    storage_account = storage_account, storage_key = storage_key,
    container = container,
    query = c(restype = "container", extra_query),
    xheaders = extra_headers
  )
}

#' Delete Container
#'
#' The Delete Container operation marks the specified container for deletion.
#' The container and any blobs contained within it are later deleted during
#' garbage collection.
#'
#' @inheritParams azure_create_storage_container
#'
#' @return
#' \code{\link[httr]{response}}
#'
#' @inheritSection azure_create_storage_container URI Parameters
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
#'   \item{x-ms-lease-id: <ID>}{Required for version 2012-02-12 and newer if the
#'   container has an active lease. To call Delete Container on a container that
#'   has an active lease, specify the lease ID in this header. If this header is
#'   not specified when there is an active lease, Delete Container will return
#'   409 (Conflict). If you specify the wrong lease ID, or a lease ID on a
#'   container that does not have an active lease, Delete Container will return
#'   412 (Precondition failed).}
#'
#'   \item{x-ms-client-request-id}{Optional. Provides a client-generated, opaque
#'   value with a 1 KB character limit that is recorded in the analytics logs
#'   when storage analytics logging is enabled. Using this header is highly
#'   recommended for correlating client-side activities with requests received
#'   by the server. For more information, see About Storage Analytics Logging
#'   and Azure Logging: Using Logs to Track Storage Requests.}
#' }
#'
#' @inheritSection azure_create_storage_container Authorization
#'
#' @section Remarks:
#' When a container is deleted, a container with the same name cannot be created
#' for at least 30 seconds; the container may not be available for more than 30
#' seconds if the service is still processing the request. While the container
#' is being deleted, attempts to create a container of the same name will fail
#' with status code 409 (Conflict), with the service returning additional error
#' information indicating that the container is being deleted. All other
#' operations, including operations on any blobs under the container, will fail
#' with status code 404 (Not Found) while the container is being deleted.
#'
#' @references
#' \url{https://docs.microsoft.com/en-us/rest/api/storageservices/delete-container}
#'
#' @export
azure_delete_storage_container <- function(
  storage_account, storage_key, container, ...,
  extra_query = new("namedList"), extra_headers = new("namedList")
) {
  # Assertions --------------------------------------------------------------
  azure_assert_call(match.call())

  # Make request ------------------------------------------------------------
  azure_blob_call(
    verb = "DELETE",
    storage_account = storage_account, storage_key = storage_key,
    container = container,
    query = c(restype = "container", extra_query),
    xheaders = extra_headers
  )
}

#' @title
#' Get Container Properties
#'
#' @description
#' The Get Container Properties operation returns all user-defined metadata
#' and system properties for the specified container. The data returned does not
#' include the container's list of blobs.
#'
#' @family container functions
#'
#' @inheritParams azure_create_storage_container
#'
#' @return
#' \code{\link[httr]{response}} object
#'
#' @inheritSection azure_create_storage_container URI Parameters
#' @section Request Headers:
#' \describe{
#'   \item{Authorization}{Required. Specifies the authentication scheme, account
#'   name, and signature. For more information, see Authentication for the Azure
#'   Storage Services.}
#'
#'   \item{Date or x-ms-date}{Required. Specifies the Coordinated Universal Time
#'   (UTC) for the request. For more information, see Authentication for the
#'   Azure Storage Services.}

#'   \item{x-ms-lease-id: <ID>}{Optional, version 2012-02-12 and newer. If
#'   specified, Get Container Properties only succeeds if the container’s
#'   lease is active and matches this ID. If there is no active lease or the ID
#'   does not match, 412 (Precondition Failed) is returned.}
#'
#'   \item{x-ms-version}{Required for all authenticated requests, optional for
#'   anonymous requests. Specifies the version of the operation to use for this
#'   request. For more information, see Versioning for the Azure Storage
#'   Services.}
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
#' If the container's access control list (ACL) is set to allow anonymous access
#' to the container, any client may call this operation. If the container is
#' private, this operation can be performed by the account owner.
#'
#' @references
#' \url{https://docs.microsoft.com/en-us/rest/api/storageservices/get-container-properties}
#'
#' @export
azure_get_container_properties <- function(
  storage_account, storage_key, container, ...,
  extra_query = new("namedList"), extra_headers = new("namedList")
) {
  # Assertions --------------------------------------------------------------
  azure_assert_call(match.call())

  # Make request ------------------------------------------------------------
  azure_blob_call(
    verb = "GET",
    storage_account = storage_account, storage_key = storage_key,
    container = container,
    query = c(restype = "container", extra_query),
    xheaders = extra_headers
  )
}

#' @title
#' Get Container Metadata
#'
#' @description
#' The Get Container Metadata operation returns all user-defined metadata for
#' the container. All the user-defined metadata are stored in the response
#' headers and starts with the "x-ms-meta-" string.
#'
#' @inheritParams azure_create_storage_container
#'
#' @return
#' \code{\link[httr]{response}} object
#'
#' @references
#' \url{https://docs.microsoft.com/en-us/rest/api/storageservices/get-container-metadata}
#'
#' @inheritSection azure_get_container_properties URI Parameters
#' @inheritSection azure_get_container_properties Request Headers
#' @section Authorization:
#' Only the account owner may call this operation.
#'
#' @section Remarks:
#' This operation returns only user-defined metadata on the container. To return
#' system properties as well, call Get Container Properties.
#'
#' @export
azure_get_container_metadata <- function(
  storage_account, storage_key, container, ...,
  extra_query = new("namedList"), extra_headers = new("namedList")
) {
  # Assertions --------------------------------------------------------------
  azure_assert_call(match.call())

  # Make request ------------------------------------------------------------
  azure_blob_call(
    verb = "GET",
    storage_account = storage_account, storage_key = storage_key,
    container = container,
    query = c(restype = "container", comp = "metadata", extra_query),
    xheaders = extra_headers
  )
}

#' @title
#' Set Container Metadata
#'
#' @description
#' The Set Container Metadata operation sets one or more user-defined
#' name-value pairs for the specified container. All user-defined metadata
#' are stored in the headers and must start with the "x-ms-meta-" string.
#'
#' @inheritParams azure_create_storage_container
#'
#' @return
#' \code{\link[httr]{response}} Object
#'
#' @inheritSection azure_get_container_properties URI Parameters
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
#'   \item{x-ms-lease-id: <ID>}{Optional, version 2012-02-12 and newer. If
#'   specified, Set Container Metadata only succeeds if the container's lease is
#'   active and matches this ID. If there is no active lease or the ID does not
#'   match, 412 (Precondition Failed) is returned.}
#'
#'   \item{x-ms-meta-name:value}{Optional. A name-value pair to associate with
#'   the container as metadata.  Each call to this operation replaces all
#'   existing metadata attached to the container. To remove all metadata from
#'   the container, call this operation with no metadata headers. Note that
#'   beginning with version 2009-09-19, metadata names must adhere to the naming
#'   rules for C# identifiers.}
#'
#'   \item{x-ms-client-request-id}{Optional. Provides a client-generated, opaque
#'   value with a 1 KB character limit that is recorded in the analytics logs
#'   when storage analytics logging is enabled. Using this header is highly
#'   recommended for correlating client-side activities with requests received
#'   by the server. For more information, see About Storage Analytics Logging
#'   and Azure Logging: Using Logs to Track Storage Requests.}
#' }
#'
#' @inheritSection azure_get_container_metadata Authorization
#'
#' @section Remarks:
#' Calling the Set Container Metadata operation overwrites all existing metadata
#' that is associated with the container. It's not possible to modify an
#' individual name-value pair.
#'
#' You may also set metadata for a container at the time it is created.
#'
#' Calling Set Container Metadata updates the ETag and Last-Modified-Time
#' properties for the container. If the request was made using version
#' 2011-08-18, the updated ETag will be in quotes.
#'
#' @references
#' \url{https://docs.microsoft.com/en-us/rest/api/storageservices/set-container-metadata}
#'
#' @export
azure_set_container_metadata <- function(
  storage_account, storage_key, container, ...,
  extra_query = new("namedList"), extra_headers = new("namedList")
) {
  # Assertions --------------------------------------------------------------
  azure_assert_call(match.call())

  # Make request ------------------------------------------------------------
  azure_blob_call(
    verb = "PUT",
    storage_account = storage_account, storage_key = storage_key,
    container = container,
    query = c(restype = "container", comp = "metadata", extra_query),
    xheaders = extra_headers
  )
}

#' @title
#' Get Container ACL (Access Control List)
#'
#' @description
#' The Get Container ACL operation gets the permissions for the specified
#' container. The permissions indicate whether container data may be accessed
#' publicly.
#'
#' @details
#' Beginning with the 2009-09-19 version, the container permissions provide the
#' following options for managing container access:
#'
#' \itemize{
#'   \item{Full public read access: Container and blob data can be read via
#'   anonymous request. Clients can enumerate blobs within the container via
#'   anonymous request, but cannot enumerate containers within the storage
#'   account.}
#'
#'   \item{Public read access for blobs only: Blob data within this container
#'   can be read via anonymous request, but container data is not available.
#'   Clients cannot enumerate blobs within the container via anonymous request.}
#'
#'   \item{No public read access: Container and blob data can be read by the
#'   account owner only.}
#' }
#'
#' Get Container ACL also returns details about any container-level access
#' policies specified on the container that may be used with Shared Access
#' Signatures. For more information, see Establishing a Stored Access Policy.
#'
#' All public access to the container is anonymous, as is access via a Shared
#' Access Signature.
#'
#' @inheritParams azure_create_storage_container
#'
#' @return
#' \code{\link[httr]{response}} object
#'
#' @inheritSection azure_get_container_properties URI Parameters
#' @inheritSection azure_get_container_properties Request Headers
#' @inheritSection azure_get_container_properties Authorization
#' @section Remarks:
#'
#' Only the account owner may read data in a particular storage account, unless
#' the account owner has specified that blobs within the container are available
#' for public read access, or made resources in the container available via a
#' Shared Access Signature.
#'
#' @references
#' \url{https://docs.microsoft.com/en-us/rest/api/storageservices/get-container-acl}
#'
#' @export
azure_get_container_acl <- function(
  storage_account, storage_key, container, ...,
  extra_query = new("namedList"), extra_headers = new("namedList")
) {
  # Assertions --------------------------------------------------------------
  azure_assert_call(match.call())

  # Make request ------------------------------------------------------------
  azure_blob_call(
    verb = "GET",
    storage_account = storage_account, storage_key = storage_key,
    container = container,
    query = c(restype = "container", comp = "acl", extra_query),
    xheaders = extra_headers
  )
}

#' @title
#' Set Container ACL (Access Control List)
#'
#' @description
#' The Set Container ACL operation sets the permissions for the specified
#' container. The permissions indicate whether blobs in a container may be
#' accessed publicly.
#'
#' @details
#' Beginning with the 2009-09-19 version, the container permissions provide the
#' following options for managing container access:
#' \itemize{
#'   \item{Full public read access: Container and blob data can be read via
#'   anonymous request. Clients can enumerate blobs within the container via
#'   anonymous request, but cannot enumerate containers within the storage
#'   account.}
#'
#'   \item{Public read access for blobs only: Blob data within this container
#'   can be read via anonymous request, but container data is not available.
#'   Clients cannot enumerate blobs within the container via anonymous request.}
#'
#'   \item{No public read access: Container and blob data can be read by the
#'   account owner only.}
#' }
#'
#' Set Container ACL also sets a stored access policy for use with shared access
#' signatures. For more information, see Establishing a Stored Access Policy.
#'
#' All public access to the container is anonymous, as is access via a shared
#' access signature.
#'
#' @inheritParams azure_create_storage_container
#' @param permissions
#' [raw|character(1)]\cr
#' Passed as request body content. See Request Body section for more
#' information.
#'
#' @inheritSection azure_get_container_acl URI Parameters
#'
#' @section Request Headers:
#' The following table describes required and optional request headers.
#' \describe{
#'  \item{Authorization}{Required. Specifies the authentication scheme, account
#'  name, and signature. For more information, see Authentication for the Azure
#'  Storage Services.}
#'
#'  \item{Date or x-ms-date}{Required. Specifies the Coordinated Universal Time
#'  (UTC) for the request. For more information, see Authentication for the
#'  Azure Storage Services.}
#'
#'  \item{x-ms-version}{Optional. Specifies the version of the operation to use
#'  for this request. For more information, see Versioning for the Azure Storage
#'  Services.}
#'
#'  \item{x-ms-blob-public-access}{Optional. Specifies whether data in the
#'  container may be accessed publicly and the level of access. Possible values
#'  include:
#'    \itemize{
#'      \item{container: Specifies full public read access for container and
#'      blob data.  Clients can enumerate blobs within the container via
#'      anonymous request, but cannot enumerate containers within the storage
#'      account.}
#'
#'      \item{blob: Specifies public read access for blobs. Blob data within
#'      this container can be read via anonymous request, but container data is
#'      not available. Clients cannot enumerate blobs within the container via
#'      anonymous request.}
#'    }
#'    If this header is not included in the request, container data is private
#'    to the account owner.  Note that setting public access for a container
#'    in an Azure Premium Storage account is not permitted.
#'  }
#'
#'  \item{x-ms-lease-id: <ID>}{Optional, version 2012-02-12 and newer. If
#'  specified, Set Container ACL only succeeds if the container's lease is
#'  active and matches this ID. If there is no active lease or the ID does not
#'  match, 412 (Precondition Failed) is returned.}
#'
#'   \item{x-ms-client-request-id}{Optional. Provides a client-generated, opaque
#'   value with a 1 KB character limit that is recorded in the analytics logs
#'   when storage analytics logging is enabled. Using this header is highly
#'   recommended for correlating client-side activities with requests received
#'   by the server. For more information, see About Storage Analytics Logging
#'   and Azure Logging: Using Logs to Track Storage Requests.}
#' }
#'
#' This operation also supports the use of conditional headers to execute the
#' operation only if a specified condition is met. For more information, see
#' Specifying Conditional Headers for Blob Service Operations.
#'
#' @section Request Body:
#' To specify a stored access policy, provide a unique identifier and access
#' policy in the request body for the Set Container ACL operation.
#'
#' The SignedIdentifier element includes the unique identifier, as specified in
#' the Id element, and the details of the access policy, as specified in the
#' AccessPolicy element. The maximum length of the unique identifier is 64
#' characters.
#'
#' The Start and Expiry fields must be expressed as UTC times and must adhere to
#' a valid ISO 8061 format. Supported ISO 8061 formats include the following:
#' \itemize{
#'   \item{YYYY-MM-DD}
#'   \item{YYYYY-MM-DDThh:mmTZD}
#'   \item{YYYYY-MM-DDThh:mm:ssTZD}
#'   \item{YYYYY-MM-DDThh:mm:ss.fffffffTZD}
#' }
#'
#' For the date portion of these formats, YYYY is a four-digit year
#' representation, MM is a two-digit month representation, and DD is a two-digit
#' day representation. For the time portion, hh is the hour representation in
#' 24-hour notation, mm is the two-digit minute representation, ss is the
#' two-digit second representation, and fffffff is the seven-digit millisecond
#' representation. A time designator T separates the date and time portions of
#' the string, while a time zone designator TZD specifies a time zone.
#'
#' @inheritSection azure_get_container_acl Authorization
#'
#' @section Remarks:
#' Only the account owner may access resources in a particular container, unless
#' the owner has specified that container resources are available for public
#' access by setting the permissions on the container, or has issued a shared
#' access signature for a resource within the container.
#'
#' When you set permissions for a container, the existing permissions are
#' replaced. To update the container's permissions, call Get Container ACL to
#' fetch all access policies associated with the container, modify the access
#' policy that you wish to change, and then call Set Container ACL with the
#' complete set of data to perform the update.
#'
#' See References for further information.
#'
#' @return
#' \code{\link[httr]{response}} object
#'
#' @references
#' \url{https://docs.microsoft.com/en-us/rest/api/storageservices/set-container-acl}
#' \url{https://stackoverflow.com/questions/6118759/better-understanding-of-purpose-of-signed-identifiers-in-blob-storage-azure}
#'
#' @export
azure_set_container_acl <- function(
  storage_account, storage_key, container, permissions, ...,
  extra_query = new("namedList"), extra_headers = new("namedList")
) {
  # Assertions --------------------------------------------------------------
  azure_assert_call(match.call())

  # Base values -------------------------------------------------------------

  # Make request ------------------------------------------------------------
  azure_blob_call(
    verb = "PUT",
    storage_account = storage_account, storage_key = storage_key,
    container = container,
    query = c(restype = "container", comp = "acl", extra_query),
    xheaders = extra_headers,
    body_content = permissions
  )
}

#' @title
#' List Blobs
#'
#' @description
#' The List Blobs operation enumerates the list of blobs under the specified
#' container.
#'
#' @inheritParams azure_create_storage_container
#'
#' @section URI Parameters:
#'
#' \describe{
#'   \item{prefix}{Optional. Filters the results to return only blobs whose
#'   names begin with the specified prefix.}
#'
#'   \item{delimiter}{Optional. When the request includes this parameter, the
#'   operation returns a BlobPrefix element in the response body that acts as a
#'   placeholder for all blobs whose names begin with the same substring up to
#'   the appearance of the delimiter character. The delimiter may be a single
#'   character or a string.}
#'
#'   \item{marker}{Optional. A string value that identifies the portion of the
#'   list to be returned with the next list operation. The operation returns a
#'   marker value within the response body if the list returned was not
#'   complete. The marker value may then be used in a subsequent call to request
#'   the next set of list items. The marker value is opaque to the client.}
#'
#'   \item{maxresults}{Optional. Specifies the maximum number of blobs to
#'   return, including all BlobPrefix elements. If the request does not specify
#'   maxresults or specifies a value greater than 5,000, the server will return
#'   up to 5,000 items. Setting maxresults to a value less than or equal to zero
#'   results in error response code 400 (Bad Request).}
#'
#'   \item{include=snapshots,metadata,uncommittedblobs,copy}{Optional. Specifies
#'   one or more datasets to include in the response:
#'     \itemize{
#'       \item{snapshots: Specifies that snapshots should be included in the
#'       enumeration. Snapshots are listed from oldest to newest in the
#'       response.}
#'
#'       \item{metadata: Specifies that blob metadata be returned in the
#'       response.}
#'
#'       \item{uncommittedblobs: Specifies that blobs for which blocks have been
#'       uploaded, but which have not been committed using Put Block List, be
#'       included in the response.}
#'
#'       \item{copy: Version 2012-02-12 and newer. Specifies that metadata
#'       related to any current or previous Copy Blob operation should be
#'       included in the response.}
#'     }
#'     To specify more than one of these options on the URI, you must separate
#'     each option with a URL-encoded comma ("%82").
#'   }
#'
#'   \item{timeout}{Optional. The timeout parameter is expressed in seconds. For
#'   more information, see Setting Timeouts for Blob Service Operations.}
#' }
#'
#' @inheritSection azure_get_container_properties Request Headers
#'
#' @section Authorization:
#' If the container's access control list (ACL) is set to allow anonymous access
#' to the container, any client may call this operation. Otherwise, this
#' operation can be called by the account owner and by anyone with a Shared
#' Access Signature that has permission to list blobs in a container.
#'
#' @section Remarks:
#' \subsection{Blob Properties in the Response}{
#'   If you have requested that uncommitted blobs be included in the
#'   enumeration, note that some properties are not set until the blob is
#'   committed, so some properties may not be returned in the response.
#'
#'   The x-ms-blob-sequence-number element is only returned for page blobs.
#'
#'   For page blobs, the value returned in the Content-Length element
#'   corresponds to the value of the blob's x-ms-blob-content-length header.
#'
#'   The Content-MD5 element appears in the response body only if it has been
#'   set on the blob using version 2009-09-19 or later. You can set the
#'   Content-MD5 property when the blob is created or by calling Set Blob
#'   Properties. In version 2012-02-12 and newer, Put Blob sets a block blob’s
#'   MD5 value even when the Put Blob request doesn’t include an MD5 header.
#' }
#'
#' \subsection{Metadata in the Response}{
#'   The Metadata element is present only if the include=metadata parameter was
#'   specified on the URI. Within the Metadata element, the value of each
#'   name-value pair is listed within an element corresponding to the pair's
#'   name.
#'
#'   Note that metadata requested with this parameter must be stored in
#'   accordance with the naming restrictions imposed by the 2009-09-19 version
#'   of the Blob service. Beginning with this version, all metadata names must
#'   adhere to the naming conventions for C# identifiers.
#'
#'   If a metadata name-value pair violates the naming restrictions enforced by
#'   the 2009-09-19 version, the response body indicates the problematic name
#'   within an x-ms-invalid-name element, as shown in the following XML
#'   fragment:
#' }
#'
#' \subsection{Snapshots in the Response}{
#'   Snapshots are listed in the response only if the include=snapshots
#'   parameter was specified on the URI. Snapshots listed in the response do not
#'   include the LeaseStatus element, as snapshots cannot have active leases.
#'
#'   If you call List Blobs with a delimiter, you cannot also include snapshots
#'   in the enumeration. A request that includes both returns an
#'   InvalidQueryParameter error (HTTP status code 400 – Bad Request).
#' }
#'
#' \subsection{Uncommitted Blobs in the Response}{
#'   Uncommitted blobs are listed in the response only if the
#'   \code{include=uncommittedblobs} parameter was specified on the URI.
#'   Uncommitted blobs listed in the response do not include any of the
#'   following elements:
#'   \itemize{
#'     \item{Last-Modified}
#'     \item{Etag}
#'     \item{Content-Type}
#'     \item{Content-Encoding}
#'     \item{Content-Language}
#'     \item{Content-MD5}
#'     \item{Cache-Control}
#'     \item{Metadata}
#'   }
#' }
#'
#' \subsection{Returning Result Sets Using a Marker Value}{
#'   If you specify a value for the maxresults parameter and the number of blobs
#'   to return exceeds this value, or exceeds the default value for maxresults,
#'   the response body will contain a NextMarker element that indicates the next
#'   blob to return on a subsequent request. To return the next set of items,
#'   specify the value of NextMarker as the marker parameter on the URI for the
#'   subsequent request.
#'
#'   Note that the value of NextMarker should be treated as opaque.
#' }
#'
#' \subsection{Using a Delimiter to Traverse the Blob Namespace}{
#'   The delimiter parameter enables the caller to traverse the blob namespace
#'   by using a user-configured delimiter. In this way, you can traverse a
#'   virtual hierarchy of blobs as though it were a file system. The delimiter
#'   may be a single character or a string. When the request includes this
#'   parameter, the operation returns a BlobPrefix element. The BlobPrefix
#'   element is returned in place of all blobs whose names begin with the same
#'   substring up to the appearance of the delimiter character. The value of the
#'   BlobPrefix element is substring+delimiter, where substring is the common
#'   substring that begins one or more blob names, and delimiter is the value of
#'   the delimiter parameter.
#'
#'   You can use the value of BlobPrefix to make a subsequent call to list the
#'   blobs that begin with this prefix, by specifying the value of BlobPrefix
#'   for the prefix parameter on the request URI.
#'
#'   Note that each BlobPrefix element returned counts toward the maximum
#'   result, just as each Blob element does.
#'
#'   Blobs are listed in alphabetical order in the response body, with
#'   upper-case letters listed first.
#' }
#'
#' \subsection{Copy errors in CopyStatusDescription}{
#'   CopyStatusDescription contains more information about the Copy Blob
#'   failure.
#'
#'   When a copy attempt fails and the Blob service is still retrying the
#'   operation, CopyStatus is set to pending, and the CopyStatusDescription text
#'   describes the failure that may have occurred during the last copy attempt.
#'
#'   When CopyStatus is set to failed, the CopyStatusDescription text describes
#'   the error that caused the copy operation to fail.
#' }
#'
#' @return
#' \code{\link[httr]{response}} object
#'
#' @export
azure_list_blobs <- function(
  storage_account, storage_key, container, ...,
  extra_query = new("namedList"), extra_headers = new("namedList")
) {
  # Assertions --------------------------------------------------------------
  azure_assert_call(
    match.call(),
    c("prefix", "timeout", "delimiter", "marker", "maxresults", "include")
  )


  # Make request ------------------------------------------------------------
  azure_blob_call(
    verb = "GET",
    storage_account = storage_account, storage_key = storage_key,
    container = container,
    query = c(restype = "container", comp = "list", extra_query),
    xheaders = extra_headers
  )
}
