library(recipes)
test_data <- tibble(text = c("I would not eat them here or there.",
                             "I would not eat them anywhere.",
                             "I would not eat green eggs and ham.",
                             "I do not like them, Sam-I-am.")
)

rec_base <- recipe(~ ., data = test_data)

# Create some manual data for expected results.
tokens <- rec_base %>% 
  step_tokenize(text) %>%
  recipes::prep(training = test_data) %>%
  recipes::juice() %>% 
  dplyr::bind_cols(test_data) %>% 
  dplyr::select(text = text1, tokens = text)

# Give each token an arbitrary value for comparison. Real embeddings will be
# doubles, so make these double.
embeddings <- tokens %>% 
  tidyr::unnest(tokens) %>% 
  dplyr::distinct(tokens) %>%
  dplyr::arrange(tokens) %>% 
  # There are 17 unique tokens. We'll represent them with a 5-d set of vectors
  # so each one can be unique.
  dplyr::mutate(
    token_num_binary = purrr::map(
      seq_along(tokens),
      function(this_token) {
        tibble(
          dimension = paste0("d", 1:5),
          score = as.double(intToBits(this_token)[1:5])
        )
      }
    )
  ) %>% 
  tidyr::unnest(token_num_binary) %>% 
  tidyr::pivot_wider(
    names_from = dimension,
    values_from = score
  )

sentence_embeddings_long <- tokens %>% 
  tidyr::unnest(tokens) %>% 
  dplyr::left_join(embeddings, by = "tokens")

# Summarize by each statistic, and reorder to original order.
sentence_embeddings_sum <- sentence_embeddings_long %>% 
  dplyr::select(-tokens) %>% 
  dplyr::group_by(text) %>% 
  dplyr::summarize_all(sum) %>% 
  dplyr::rename_if(
    is.numeric,
    ~ paste("w_embed", "sum", ., sep = "_")
  )
sentence_embeddings_sum <- test_data %>% 
  dplyr::left_join(sentence_embeddings_sum, by = "text")

sentence_embeddings_mean <- sentence_embeddings_long %>% 
  dplyr::select(-tokens) %>% 
  dplyr::group_by(text) %>% 
  dplyr::summarize_all(mean) %>% 
  dplyr::rename_if(
    is.numeric,
    ~ paste("w_embed", "mean", ., sep = "_")
  )
sentence_embeddings_mean <- test_data %>% 
  dplyr::left_join(sentence_embeddings_mean, by = "text")

sentence_embeddings_min <- sentence_embeddings_long %>% 
  dplyr::select(-tokens) %>% 
  dplyr::group_by(text) %>% 
  dplyr::summarize_all(min) %>% 
  dplyr::rename_if(
    is.numeric,
    ~ paste("w_embed", "min", ., sep = "_")
  )
sentence_embeddings_min <- test_data %>% 
  dplyr::left_join(sentence_embeddings_min, by = "text")

sentence_embeddings_max <- sentence_embeddings_long %>% 
  dplyr::select(-tokens) %>% 
  dplyr::group_by(text) %>% 
  dplyr::summarize_all(max) %>% 
  dplyr::rename_if(
    is.numeric,
    ~ paste("w_embed", "max", ., sep = "_")
  )
sentence_embeddings_max <- test_data %>% 
  dplyr::left_join(sentence_embeddings_max, by = "text")

rec <- rec_base %>%
  step_tokenize(text) %>%
  step_word_embeddings(text, embeddings = embeddings)

obj <- rec %>%
  prep(training = test_data, retain = TRUE)

juiced <- juice(obj)

test_that("step_word_embeddings adds the appropriate number of columns.", {
  ncol_given <- ncol(embeddings) - 1L
  ncol_juiced <- juiced %>% 
    select(contains("w_embed_")) %>% 
    ncol()
  expect_identical(ncol_juiced, ncol_given)
})


test_that("step_word_embeddings gives numeric output.", {
  expect_true(
    juiced %>%
      select(contains("embedding")) %>%
      lapply(is.numeric) %>%
      unlist() %>%
      all()
  )
  
  # Need to dig in and grok what impacts this part before I finalize the test.
  # expect_equal(dim(tidy(rec, 2)), c(1, 4))
  # expect_equal(dim(tidy(obj, 2)), c(1, 4))
})

test_that("step_word_embeddings aggregates vectors as expected.", {
  # By default, step_word_embeddings sums the vectors of the tokens it is given.
  expect_identical(
    juiced, select(sentence_embeddings_sum, -text)
  )
  
  # Also allow the user to choose an aggregation function.
  juiced_max <- rec_base %>% 
    step_tokenize(text) %>%
    step_word_embeddings(
      text, embeddings = embeddings, aggregation = "max"
    ) %>% 
    prep(training = test_data, retain = TRUE) %>% 
    juice()
  expect_identical(juiced_max, select(sentence_embeddings_max, -text))
  juiced_min <- rec_base %>% 
    step_tokenize(text) %>%
    step_word_embeddings(
      text, embeddings = embeddings, aggregation = "min"
    ) %>% 
    prep(training = test_data, retain = TRUE) %>% 
    juice()
  expect_identical(juiced_min, select(sentence_embeddings_min, -text))
  juiced_mean <- rec_base %>% 
    step_tokenize(text) %>%
    step_word_embeddings(
      text, embeddings = embeddings, aggregation = "mean"
    ) %>% 
    prep(training = test_data, retain = TRUE) %>% 
    juice()
  expect_identical(juiced_mean, select(sentence_embeddings_mean, -text))
})

# test_that("step_word_embeddings deals with missing words appropriately.", {
#   # Warn if some of the words we're trying to match aren't in the embeddings
#   # tibble.
#   
#   # Error if none of the words for a given row are in the embeddings tibble.
# })
