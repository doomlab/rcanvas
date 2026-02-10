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
