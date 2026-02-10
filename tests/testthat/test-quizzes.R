context("quiz helpers")

test_that("split_quiz_question_answers flattens answers", {
  quiz_questions <- data.frame(
    id = c(101, 102),
    question_name = c("Alpha", "Beta"),
    answers = I(list(
      data.frame(id = c(11, 12), text = c("red", "blue"), stringsAsFactors = FALSE),
      data.frame(id = 21, text = "green", stringsAsFactors = FALSE)
    )),
    stringsAsFactors = FALSE
  )

  split <- split_quiz_question_answers(quiz_questions)

  expect_is(split, "list")
  expect_named(split, c("questions", "question_answers"))
  expect_equal(nrow(split$questions), 2L)

  expect_equal(split$question_answers$question_id, c(101L, 101L, 102L))
  expect_equal(split$question_answers$answer_id, c(11L, 12L, 21L))
  expect_equal(split$question_answers$text, c("red", "blue", "green"))
})

test_that("resolve_submission_quiz_answers joins metadata", {
  quiz_questions <- data.frame(
    id = c(101, 102),
    question_name = c("Alpha", "Beta"),
    answers = I(list(
      data.frame(id = c(11, 12), text = c("red", "blue"), stringsAsFactors = FALSE),
      data.frame(id = 21, text = "green", stringsAsFactors = FALSE)
    )),
    stringsAsFactors = FALSE
  )

  submission <- list(
    submission_history = list(
      list(submission_data = list(
        data.frame(question_id = c(101, 102), answer_id = c(11, 21),
                   text = c("red?", "green?"), stringsAsFactors = FALSE)
      ))
    )
  )

  resolved <- resolve_submission_quiz_answers(submission, quiz_questions)

  expect_named(resolved, c("submission", "submission_data", "questions", "question_answers"))
  expect_equal(resolved$submission_data$question_id, c(101L, 102L))
  expect_equal(resolved$submission_data$answer_id, c(11L, 21L))
  expect_true("option_text" %in% names(resolved$submission_data))
  expect_equal(resolved$submission_data$option_text, c("red", "green"))
})
