context("S3 methods")

# ------------------------------------------------------------------------------

library(recipes)
library(textrecipes)

test_data <- tibble(text = c(
  "I would not eat them here or there.",
  "I would not eat them anywhere.",
  "I would not eat green eggs and ham.",
  "I do not like them, Sam-I-am."
))

embeddings <- tibble(
  tokens = c("green", "eggs", "ham"),
  d1 = c(1, 0, 0),
  d2 = c(0, 1, 0),
  d3 = c(0, 0, 1)
)


r01 <- recipe(~., data = test_data)
r02 <- r01 %>% step_text_normalization(text)
r03 <- r01 %>% step_textfeature(text)
r04 <- r01 %>% step_lda(text)
r05 <- r01 %>% step_tokenize(text)

r06 <- r05 %>% step_untokenize(text)
r07 <- r05 %>% step_tf(text)
r08 <- r05 %>% step_tfidf(text)
r09 <- r05 %>% step_sequence_onehot(text)
r10 <- r05 %>% step_texthash(text)
r11 <- r05 %>% step_word_embeddings(text, embeddings = embeddings)

r11 <- r05 %>% step_stem(text)
r12 <- r05 %>% step_stopwords(text)
r13 <- r05 %>% step_lemma(text)
r14 <- r05 %>% step_tokenfilter(text, min_times = 2)
r15 <- r05 %>% step_pos_filter(text)
r16 <- r05 %>% step_ngram(text)

nothing <-
  tibble::tibble(
    name = character(0),
    call_info = list(),
    source = character(0),
    component = character(0),
    component_id = character(0)
  )

# ------------------------------------------------------------------------------

test_that("required packages", {
  expect_equal(required_pkgs(r01), "recipes")
  expect_equal(required_pkgs(r02), c("recipes", "stringi", "textrecipes"))
  expect_equal(required_pkgs(r03), c("recipes", "textfeatures", "textrecipes"))
  expect_equal(required_pkgs(r04), c("recipes", "textfeatures", "textrecipes"))
  expect_equal(required_pkgs(r05), c("recipes", "textrecipes"))
  expect_equal(required_pkgs(r06), c("recipes", "textrecipes"))
  expect_equal(required_pkgs(r07), c("recipes", "textrecipes"))
  expect_equal(required_pkgs(r08), c("recipes", "textrecipes"))
  expect_equal(required_pkgs(r09), c("recipes", "textrecipes"))
  expect_equal(required_pkgs(r10), c("recipes", "textrecipes", "text2vec"))
  expect_equal(required_pkgs(r11), c("recipes", "textrecipes", "SnowballC"))
  expect_equal(required_pkgs(r12), c("recipes", "textrecipes", "stopwords"))
  expect_equal(required_pkgs(r13), c("recipes", "textrecipes"))
  expect_equal(required_pkgs(r14), c("recipes", "textrecipes"))
  expect_equal(required_pkgs(r15), c("recipes", "textrecipes"))
  expect_equal(required_pkgs(r16), c("recipes", "textrecipes"))
})

test_that("tunable arguments", {
  expect_equal(tunable(r01), nothing)
  expect_equal(tunable(r02), nothing)
  expect_equal(tunable(r03), nothing)
  expect_equal(tunable(r04), nothing)
  expect_equal(tunable(r05)$name, "token")
  expect_equal(tunable(r06)$name, "token")
  expect_equal(tunable(r07)$name, c("token", "weight_scheme", "num_terms"))
  expect_equal(tunable(r08)$name, "token")
  expect_equal(tunable(r09)$name, "token")
  expect_equal(tunable(r10)$name, c("token", "signed", "num_terms"))
  expect_equal(tunable(r11)$name, "token")
  expect_equal(tunable(r12)$name, "token")
  expect_equal(tunable(r13)$name, "token")
  expect_equal(
    tunable(r14)$name,
    c("token", "max_times", "min_times", "max_tokens")
  )
  expect_equal(tunable(r15)$name, "token")
  expect_equal(tunable(r16)$name, c("token", "num_tokens"))
})
