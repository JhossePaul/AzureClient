#' Azure call assertion
#'
#' Checks if the function call is valid
#'
#' @param function_call      [\code{\link[base]{call}}]\cr
#' A function with the params to check
#'
#' @param valid_query_params [character(n)]\cr
#' Valid URI parameters for the request
#'
#' @return [logical(1)]\cr
#' TRUE if all assertions passed else throw an error.
#'
#' @note
#' Forgive me God, for this chunk of metaprogramming. Any better ideas are
#' welcomed.
azure_assert_call <- function (function_call, valid_query_params = "timeout") {
  # Call as a list -----------------------------------------------------------
  params <- function_call %>% as.list %>% extract(-1L) %>% lapply(eval)

  # Required arguments' assertions --------------------------------------------
  assert_character(params$storage_account, null.ok = FALSE)
  assert_character(params$storage_key, null.ok = FALSE)

  # Optional arguments' assertions
  assert_list(
    params$extra_query,
    types = "character", names = "unique",
    any.missing = FALSE, null.ok = TRUE
  )
  assert_list(
    params$extra_headers,
    types = "character", names = "unique",
    any.missing = FALSE, null.ok = TRUE
  )
  assert(
    check_names(
      names(params$extra_query),
      type = "unique",
      subset.of = valid_query_params
    ),
    check_null(names(params$extra_query))
  )

  return(TRUE)
}
