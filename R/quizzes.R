#' Split quiz questions into metadata and answer mappings
#'
#' Canvas returns each quiz question with an `answers` list column that
#' contains every multiple choice option (with their own `id` and `text`).
#' `split_quiz_question_answers()` removes the nested column and returns a list
#' with `questions` (one row per question) and `question_answers` (one row per
#' answer, with `question_id` and `answer_id` for easy joins).
#'
#' @param quiz_questions data frame returned by `/courses/{course_id}/quizzes/{quiz_id}/questions`.
#'    The object must contain a question identifier (`id` or `question_id`) and
#'    an `answers` column.
#' @return Named list with `questions` (metadata without the `answers` column)
#'    and `question_answers` (flattened answer rows).
#' @export
#' @examples
#' questions <- data.frame(
#'   id = c(1, 2),
#'   question_name = c("First", "Second"),
#'   answers = I(list(
#'     data.frame(id = c(10, 11), text = c("A", "B"), stringsAsFactors = FALSE),
#'     data.frame(id = 20, text = "C", stringsAsFactors = FALSE)
#'   )),
#'   stringsAsFactors = FALSE
#' )
#' split_quiz_question_answers(questions)
split_quiz_question_answers <- function(quiz_questions) {
  if (!is.data.frame(quiz_questions)) {
    quiz_questions <- as.data.frame(quiz_questions, stringsAsFactors = FALSE)
  }

  if (!"answers" %in% names(quiz_questions)) {
    stop("quiz_questions must include an 'answers' column", call. = FALSE)
  }

  question_id_candidates <- intersect(c("question_id", "id"), names(quiz_questions))
  if (length(question_id_candidates) == 0L) {
    stop("quiz_questions must include 'id' or 'question_id'", call. = FALSE)
  }
  question_id_col <- question_id_candidates[[1]]

  questions <- quiz_questions
  questions[["answers"]] <- NULL

  answer_map <- data.frame(
    question_id = quiz_questions[[question_id_col]],
    stringsAsFactors = FALSE
  )
  answer_map[["answers"]] <- quiz_questions[["answers"]]
  answer_map <- tidyr::unnest(answer_map, cols = "answers")

  if ("id" %in% names(answer_map)) {
    names(answer_map)[names(answer_map) == "id"] <- "answer_id"
  }

  list(questions = questions, question_answers = answer_map)
}

#' Join submission history with quiz question metadata
#'
#' `resolve_submission_quiz_answers()` takes the raw return value from
#' `get_submission_single()` plus the quiz question payload and returns a list
#' that flattens submission changes, attaches the question definition, and
#' exposes the option text for each `answer_id`.
#'
#' @param submission A Canvas submission object (e.g., the return value of `get_submission_single()`).
#' @param quiz_questions Output from `/courses/{course_id}/quizzes/{quiz_id}/questions`.
#' @return List with `submission` (original object), `submission_data` (per-question rows),
#'   `questions` (metadata), and `question_answers` (flattened option map).
#' @export
#' @examples
#' quiz_questions <- data.frame(
#'   id = c(101, 102),
#'   question_name = c("Alpha", "Beta"),
#'   answers = I(list(
#'     data.frame(id = c(11, 12), text = c("red", "blue"), stringsAsFactors = FALSE),
#'     data.frame(id = 21, text = "green", stringsAsFactors = FALSE)
#'   )),
#'   stringsAsFactors = FALSE
#' )
#' submission <- list(
#'   submission_history = list(
#'     list(submission_data = list(
#'       data.frame(question_id = c(101, 102), answer_id = c(11, 21),
#'                  text = c("red?", "green?"), stringsAsFactors = FALSE)
#'     ))
#'   )
#' )
#' resolve_submission_quiz_answers(submission, quiz_questions)
resolve_submission_quiz_answers <- function(submission, quiz_questions) {
  if (!is.list(submission)) {
    stop("submission must be a list (as produced by get_submission_single)", call. = FALSE)
  }

  history <- submission$submission_history[[1]]
  if (is.null(history) || is.null(history$submission_data[[1]])) {
    stop("submission_history/submission_data not found", call. = FALSE)
  }

  submission_data <- history$submission_data[[1]]
  if (!is.data.frame(submission_data)) {
    submission_data <- as.data.frame(submission_data, stringsAsFactors = FALSE)
  }

  rename_col <- function(df, candidates, new_name) {
    keep <- intersect(candidates, names(df))
    if (length(keep) == 0L) {
      return(df)
    }
    names(df)[names(df) == keep[[1]]] <- new_name
    df
  }

  submission_data <- rename_col(submission_data, c("question_id", "question_definition_id", "id"), "question_id")
  if (!"question_id" %in% names(submission_data)) {
    stop("submission_data must contain a question identifier", call. = FALSE)
  }
  submission_data <- rename_col(submission_data, c("answer_id", "answer_ids", "selected_answer_id"), "answer_id")

  split <- split_quiz_question_answers(quiz_questions)

  questions <- split$questions
  questions <- rename_col(questions, c("question_id", "id"), "question_id")
  if (!"question_id" %in% names(questions)) {
    stop("quiz_questions must include a question identifier column", call. = FALSE)
  }

  question_answers <- split$question_answers
  if ("text" %in% names(question_answers)) {
    question_answers$option_text <- question_answers$text
    question_answers$text <- NULL
  }

  submission_data <- dplyr::left_join(submission_data, questions, by = "question_id")
  if ("answer_id" %in% names(submission_data)) {
    submission_data <- dplyr::left_join(submission_data, question_answers,
                                        by = c("question_id", "answer_id"))
  }

  list(
    submission = submission,
    submission_data = submission_data,
    questions = questions,
    question_answers = question_answers
  )
}
