library(recipes)
library(textrecipes)
library(tibble)

set.seed(123)

# Large embedding matrix: 50k vocab, 100 dims
n_vocab <- 50000
n_dim <- 100
vocab <- paste0("word", seq_len(n_vocab))
emb_mat <- matrix(rnorm(n_vocab * n_dim), nrow = n_vocab)
embeddings <- tibble::as_tibble(emb_mat, .name_repair = "minimal")
names(embeddings) <- paste0("d", seq_len(n_dim))
embeddings <- tibble::add_column(embeddings, tokens = vocab, .before = 1)

# Sample data: 5000 documents, ~20 tokens each drawn from vocab
n_doc <- 5000
make_doc <- function() {
  k <- sample(10:30, 1)
  paste(sample(vocab, k, replace = TRUE), collapse = " ")
}
sample_data <- tibble(
  text = vapply(seq_len(n_doc), function(i) make_doc(), character(1)),
  text_label = sample(c("a", "b"), n_doc, replace = TRUE)
)

rec <- recipe(text_label ~ ., data = sample_data) |>
  step_tokenize(text) |>
  step_word_embeddings(text, embeddings = embeddings)
