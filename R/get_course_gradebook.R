#' Get a course gradebook
#'
#' @importFrom magrittr %>%
#'
#' @param course_id A valid course id
#' @param progress logical; print page numbers as we go? (Defaults to FALSE)
#'
#' @return A gradebook (in long format)
#'
#' @examples
#' \dontrun{get_course_gradebook(20)}
#'
#' @export
get_course_gradebook <- function(course_id, progress = FALSE) {
  course_assignments <- get_course_items(course_id, "assignments")

  students <- get_course_items(course_id, "enrollments") %>%
    dplyr::filter(type == "StudentEnrollment", user.name != "Test Student") %>%
    dplyr::select(user.name, user_id, sis_user_id, user.login_id,
                  grades.final_score, course_id) %>%
    unique()

  n_pages <- ceiling(nrow(students)*nrow(course_assignments)/1000) + 1

  gradebook <- purrr::map_df(seq_len(n_pages), function(page) {
    if(progress)
      cat(page, "of", n_pages, "\n")

    submissions <- purrr::pmap_dfr(list(course_id, course_assignments$id, page),
                                   get_assignment_submissions)
    if (nrow(submissions) > 0){
      gradebook_page <- dplyr::left_join(students, submissions, by = "user_id") %>%
        dplyr::left_join(course_assignments %>%
                           dplyr::select(id, assignment_name = name),
                         by = c("assignment_id" = "id"))
      return(gradebook_page)
      }
  })

  return(gradebook)
}

#' @export
get_assignment_submissions <- function(course_id, assignment_id, page) {
  url <- sprintf("%s/courses/%s/assignments/%s/submissions",
                 canvas_url(), course_id, assignment_id)
  submissions <- canvas_query(url, args = list(per_page = 1000, page = page)) %>%
    httr::content("text") %>%
    jsonlite::fromJSON(flatten = TRUE)
  return(submissions)
}
