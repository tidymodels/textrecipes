library(textrecipes)
library(recipes)

skip_if_no_python_or_no_spacy <- function() {
  if (spacyr::find_spacy_env()) return(NULL)
  spacy_path <- spacyr::find_spacy(ask = FALSE)
  if (is.null(spacy_path)) {
    testthat::skip("Skip the test as spaCy is not found")
  } else if (is.na(spacy_path)) {
    testthat::skip("Skip the test as python is not found")
  }
}

text <- c("I would not eat them here or there.",
          "I would not eat them anywhere.",
          "I would not eat green eggs and ham.",
          "I do not like them, Sam-I-am.")

test_that("tokenizer works", {
  skip_on_cran()
  skip_if_no_python_or_no_spacy()
  out <- textrecipes:::spacyr_tokenizer_words(text)
  
  expect_s3_class(out, "textrecipes_tokenlist")
  
  expect_equal(
    vctrs::vec_data(out),
    list(c("I", "would", "not", "eat", "them", "here", "or", "there", "."),
         c("I", "would", "not", "eat", "them", "anywhere", "."),
         c("I", "would", "not", "eat", "green", "eggs", "and", "ham", "."),
         c("I", "do", "not", "like", "them", ",", "Sam", "-", "I", "-", "am", "."))
  )
})

