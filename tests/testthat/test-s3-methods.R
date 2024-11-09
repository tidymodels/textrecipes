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

# ------------------------------------------------------------------------------

test_that("required packages", {
  expect_equal(required_pkgs(r01), "recipes")
  expect_equal(required_pkgs(r02), c("recipes", "stringi", "textrecipes"))
  expect_equal(required_pkgs(r03), c("recipes", "textrecipes"))
  expect_equal(required_pkgs(r04), c("recipes", "textrecipes"))
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
