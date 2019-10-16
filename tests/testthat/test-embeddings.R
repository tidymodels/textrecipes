library(recipes)
test_data <- tibble(text = c("I would not eat them here or there.",
                             "I would not eat them anywhere.",
                             "I would not eat green eggs and ham.",
                             "I do not like them, Sam-I-am.")
)

rec_base <- recipe(~ ., data = test_data)

# unique_tokens <- rec %>% 
#   step_tokenize(text) %>% 
#   recipes::prep(training = test_data) %>% 
#   recipes::juice() %>% 
#   dplyr::pull(text) %>% 
#   unlist() %>% 
#   unique() %>% 
#   sort()
# embeddings <- dplyr::filter(
#   textdata::embedding_glove6b(dimensions = 50), 
#   token %in% unique_tokens
# )
# saveRDS(embeddings, here::here("tests", "testthat", "embeddings.rds"))
embeddings <- readRDS(here::here("tests", "testthat", "embeddings.rds"))

rec <- rec_base %>%
  step_tokenize(text) %>%
  step_embeddings(text, embeddings = embeddings)

obj <- rec %>%
  prep(training = test_data, retain = TRUE)

juiced <- juice(obj)

test_that("step_embeddings adds the appropriate number of columns.", {
  ncol_given <- ncol(embeddings) - 1
  ncol_juiced <- juiced %>% 
    select(contains("embedding")) %>% 
    ncol()
  expect_identical(ncol_juiced, ncol_given)
})


test_that("step_embeddings gives double output.", {
  expect_true(
    juiced %>%
      select(contains("embedding")) %>%
      lapply(is.double) %>%
      unlist() %>%
      all()
  )
  
  # Need to dig in and grok what impacts this part before I finalize the test.
  expect_equal(dim(tidy(rec, 2)), c(1, 4))
  expect_equal(dim(tidy(obj, 2)), c(1, 4))
})

test_that("step_embeddings aggregates vectors as expected.", {
  # By default, step_embeddings sums the vectors of the tokens it is given.
  expected_vectors <- "to do"
  expect_identical(
    juiced, expected_vectors
  )
  
  # Also allow the user to supply an aggregation function.
  expected_vectors <- "to do"
  juiced_max <- rec_base %>% 
    step_tokenize(text) %>%
    step_embeddings(text, embeddings = embeddings, aggregation = max) %>% 
    prep(training = test_data, retain = TRUE) %>% 
    juice()
  expect_identical(juiced_max, expected_vectors)
})

test_that("step_embeddings deals with missing words appropriately.", {
  # Warn if some of the words we're trying to match aren't in the embeddings
  # tibble.
  
  # Error if none of the words for a given row are in the embeddings tibble.
})
