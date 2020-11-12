library(textrecipes)
library(recipes)

text1 <- c(
  "I would not eat them here or there.",
  "I would not eat them anywhere.",
  "I would not eat green eggs and ham.",
  "I do not like them, Sam-I-am."
)

text2 <- c(
  "You would not eat them here or there.",
  "You would not eat them anywhere.",
  "You would not eat green eggs and ham.",
  "You do not like them, Sam-I-am."
)

test_data <- tibble(text1, text2)

text1_out <- list(
  c(
    "\U2581I", "\U2581would", "\U2581not", "\U2581\U0065\U0061\U0074",
    "\U2581them", "\U2581here", "\U2581or", "\U2581there."
  ),
  c(
    "\U2581I", "\U2581would", "\U2581not", "\U2581\U0065\U0061\U0074",
    "\U2581them",
    "\U2581\U0061\U006E\U0079\U0077\U0068\U0065\U0072\U0065\U002E"
  ),
  c(
    "\U2581I", "\U2581would", "\U2581not", "\U2581\U0065\U0061\U0074",
    "\U2581green", "\U2581\U0065\U0067\U0067\U0073",
    "\U2581\U0061\U006E\U0064", "\U2581ham."
  ),
  c(
    "\U2581I", "\U2581\U0064\U006F", "\U2581not", "\U2581like",
    "\U2581them,", "\U2581Sam-I-am."
  )
)

text2_out <- list(
  c(
    "\U2581You", "\U2581would", "\U2581not", "\U2581\U0065\U0061\U0074",
    "\U2581them", "\U2581here", "\U2581or", "\U2581there."
  ),
  c(
    "\U2581You", "\U2581would", "\U2581not", "\U2581\U0065\U0061\U0074",
    "\U2581them",
    "\U2581\U0061\U006E\U0079\U0077\U0068\U0065\U0072\U0065\U002E"
  ),
  c(
    "\U2581You", "\U2581would", "\U2581not", "\U2581\U0065\U0061\U0074",
    "\U2581green", "\U2581\U0065\U0067\U0067\U0073",
    "\U2581\U0061\U006E\U0064", "\U2581ham."
  ),
  c(
    "\U2581You", "\U2581\U0064\U006F", "\U2581not", "\U2581like",
    "\U2581them,", "\U2581Sam-I-am."
  )
)

test_that("tokenizer works", {
  skip_if_not_installed("tokenizers.bpe")

  fun1 <- tokenizers_bpe_words(text1)

  out <- fun1(text1)

  expect_s3_class(out, "textrecipes_tokenlist")

  expect_equal(
    vctrs::field(out, "tokens"),
    text1_out
  )

  expect_error(
    vctrs::field(out, "lemma")
  )
  expect_error(
    vctrs::field(out, "pos")
  )

  fun2 <- tokenizers_bpe_words(text2)

  out <- fun2(text2)

  expect_s3_class(out, "textrecipes_tokenlist")

  expect_equal(
    vctrs::field(out, "tokens"),
    text2_out
  )
})

test_that("step_tokenize works with tokenizers.bpe", {
  res <- recipe(~text1, data = test_data) %>%
    step_tokenize(text1, engine = "tokenizers.bpe") %>%
    prep() %>%
    bake(new_data = NULL)

  expect_equal(
    vctrs::field(res$text1, "tokens"),
    text1_out
  )
})

test_that("step_tokenize works with tokenizers.bpe and multiple colunms", {
  res <- recipe(~., data = test_data) %>%
    step_tokenize(all_predictors(), engine = "tokenizers.bpe") %>%
    prep() %>%
    bake(new_data = NULL)

  expect_equal(
    vctrs::field(res$text1, "tokens"),
    text1_out
  )

  expect_equal(
    vctrs::field(res$text2, "tokens"),
    text2_out
  )
})

test_that("arguments are passed to tokenizers.bpe", {
  res <- recipe(~text1, data = test_data) %>%
    step_tokenize(text1,
      engine = "tokenizers.bpe",
      training_options = list(vocab_size = 60)
    ) %>%
    prep() %>%
    bake(new_data = NULL)

  expect_equal(
    length(textrecipes:::get_unique_tokens(res$text1)),
    60
  )

  res <- recipe(~text1, data = test_data) %>%
    step_tokenize(text1,
      engine = "tokenizers.bpe",
      training_options = list(vocab_size = 80)
    ) %>%
    prep() %>%
    bake(new_data = NULL)

  expect_equal(
    length(textrecipes:::get_unique_tokens(res$text1)),
    80
  )
})
