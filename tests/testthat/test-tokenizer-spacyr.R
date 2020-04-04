library(textrecipes)
library(recipes)

text <- c("I would not eat them here or there.",
          "I would not eat them anywhere.",
          "I would not eat green eggs and ham.",
          "I do not like them, Sam-I-am.")

test_that("tokenizer works", {
  skip_on_cran()
  skip_if_no_python_or_no_spacy()
  out <- spacyr_tokenizer_words(text)
  
  expect_s3_class(out, "textrecipes_tokenlist")
  
  expect_equal(
    vctrs::vec_data(out),
    list(c("I", "would", "not", "eat", "them", "here", "or", "there", "."),
         c("I", "would", "not", "eat", "them", "anywhere", "."),
         c("I", "would", "not", "eat", "green", "eggs", "and", "ham", "."),
         c("I", "do", "not", "like", "them", ",", "Sam", "-", "I", "-", "am", "."))
  )
  
  expect_equal(
    lengths(attr(out, "lemma")),
    lengths(vctrs::vec_data(out))
  )
  
  expect_equal(
    lengths(attr(out, "pos")),
    lengths(vctrs::vec_data(out))
  )
  
  
  expect_false(
    isTRUE(all.equal(
      attr(out, "lemma"),
      vctrs::vec_data(out)
    ))
  )
  
  expect_false(
    isTRUE(all.equal(
      attr(out, "pos"),
      vctrs::vec_data(out)
    ))
  )
})

