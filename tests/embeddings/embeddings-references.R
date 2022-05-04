library(textrecipes)
library(testthat)

test_data <- tibble(text = c(
  "I would not eat them here or there.",
  "I would not eat them anywhere.",
  "I would not eat green eggs and ham.",
  "I do not like them, Sam-I-am."
))

rec_base <- recipe(~., data = test_data)

# Create some manual data for expected results.
tokens <- rec_base %>%
  step_tokenize(text) %>%
  recipes::prep() %>%
  recipes::bake(new_data = NULL) %>%
  vctrs::vec_cbind(rename(test_data, text1 = text)) %>%
  dplyr::select(text = text1, tokens = text)

# Give each token an arbitrary value for comparison. Real embeddings will be
# doubles, so make these double.
embeddings <- tokens %>%
  dplyr::mutate(tokens = vctrs::field(tokens, "tokens")) %>%
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

saveRDS(embeddings, test_path("emb-data", "embeddings.rds"), version = 2)

sentence_embeddings_long <- tokens %>%
  dplyr::mutate(tokens = vctrs::field(tokens, "tokens")) %>%
  tidyr::unnest(tokens) %>%
  dplyr::left_join(embeddings, by = "tokens")

saveRDS(sentence_embeddings_long, test_path("emb-data", "long.rds"), version = 2)

# Summarize by each statistic, and reorder to original order.
sentence_embeddings_sum <- sentence_embeddings_long %>%
  dplyr::select(-tokens) %>%
  dplyr::group_by(text) %>%
  dplyr::summarize_all(sum) %>%
  dplyr::rename_if(
    is.numeric,
    ~ paste("wordembed_text", ., sep = "_")
  )

sentence_embeddings_sum <- test_data %>%
  dplyr::left_join(sentence_embeddings_sum, by = "text")

saveRDS(sentence_embeddings_sum, test_path("emb-data", "sum.rds"), version = 2)

sentence_embeddings_mean <- sentence_embeddings_long %>%
  dplyr::select(-tokens) %>%
  dplyr::group_by(text) %>%
  dplyr::summarize_all(mean) %>%
  dplyr::rename_if(
    is.numeric,
    ~ paste("wordembed_text", ., sep = "_")
  )
sentence_embeddings_mean <- test_data %>%
  dplyr::left_join(sentence_embeddings_mean, by = "text")

saveRDS(sentence_embeddings_mean, test_path("emb-data", "mean.rds"), version = 2)

sentence_embeddings_min <- sentence_embeddings_long %>%
  dplyr::select(-tokens) %>%
  dplyr::group_by(text) %>%
  dplyr::summarize_all(min) %>%
  dplyr::rename_if(
    is.numeric,
    ~ paste("wordembed_text", ., sep = "_")
  )
sentence_embeddings_min <- test_data %>%
  dplyr::left_join(sentence_embeddings_min, by = "text")

saveRDS(sentence_embeddings_min, test_path("emb-data", "min.rds"), version = 2)

sentence_embeddings_max <- sentence_embeddings_long %>%
  dplyr::select(-tokens) %>%
  dplyr::group_by(text) %>%
  dplyr::summarize_all(max) %>%
  dplyr::rename_if(
    is.numeric,
    ~ paste("wordembed_text", ., sep = "_")
  )
sentence_embeddings_max <- test_data %>%
  dplyr::left_join(sentence_embeddings_max, by = "text")

saveRDS(sentence_embeddings_max, test_path("emb-data", "max.rds"), version = 2)
