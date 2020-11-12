library(textrecipes)
library(recipes)

text <- c(
  "I would not eat them here or there.",
  "I would not eat them anywhere.",
  "I would not eat green eggs and ham.",
  "I do not like them, Sam-I-am."
)

test_that("tokenizer works", {
  skip_on_cran()
  skip_if_no_python_or_no_spacy()
  out <- spacyr_tokenizer_words(text)

  expect_s3_class(out, "textrecipes_tokenlist")

  expect_equal(
    vctrs::field(out, "tokens"),
    list(
      c("I", "would", "not", "eat", "them", "here", "or", "there", "."),
      c("I", "would", "not", "eat", "them", "anywhere", "."),
      c("I", "would", "not", "eat", "green", "eggs", "and", "ham", "."),
      c("I", "do", "not", "like", "them", ",", "Sam", "-", "I", "-", "am", ".")
    )
  )

  expect_equal(
    lengths(vctrs::field(out, "lemma")),
    lengths(vctrs::field(out, "tokens"))
  )

  expect_equal(
    lengths(vctrs::field(out, "pos")),
    lengths(vctrs::field(out, "tokens"))
  )


  expect_false(
    isTRUE(all.equal(
      vctrs::field(out, "lemma"),
      vctrs::field(out, "tokens")
    ))
  )

  expect_false(
    isTRUE(all.equal(
      vctrs::field(out, "pos"),
      vctrs::field(out, "tokens")
    ))
  )
})
