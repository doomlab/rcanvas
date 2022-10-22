#' Get various user items
#'
#' @param user_id A valid canvas user id
#' @param item One of "missing_submissions", "details", "profile", "page_views", "colors", or "avatars"
#'
#' @return data frame
#' @export
#'
#' @examples
#'
#' \dontrun{get_user_items(365, "details")}
get_user_items <- function(user_id, item) {
  if (item == "page_views") warning("Not all page views will be returned.")

  if (item == "details") {
    url <- make_canvas_url("users", user_id)
  } else {
    url <- make_canvas_url("users", user_id, item)
  }

  args <- list(access_token = check_token(),
               per_page = 100)

  dat <- process_response(url, args)
  return(dat)
}

#' @export
get_all_users <- function(acc_id = 1, include = NULL) {

  url <- make_canvas_url("accounts", acc_id, "users")

  args <- list(
    per_page = 100,
    acc_id = acc_id
  )
  include <- iter_args_list(include, "include[]")
  args <- c(args, include)
  dat <- process_response(url, args)
  return(unique(dat))
}
