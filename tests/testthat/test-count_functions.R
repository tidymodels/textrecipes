test_that("first_person counts words, not whole-document matches (#316)", {
  expect_equal(
    first_person(c(
      "I would not eat them here or there.",
      "I would not eat them anywhere."
    )),
    c(1L, 1L)
  )
  expect_equal(first_person("This is mine, my friend."), 3L)
})

test_that("first_personp counts words, not whole-document matches (#316)", {
  expect_equal(
    first_personp(c("We are the champions", "these are ours")),
    c(1L, 2L)
  )
})

test_that("second_person counts words, not whole-document matches (#316)", {
  expect_equal(
    second_person(c("Is this your book?", "I love yourself")),
    c(1L, 1L)
  )
})

test_that("second_personp counts words, not whole-document matches (#316)", {
  expect_equal(
    second_personp(c("He gave it to her.", "Its hers now.")),
    c(2L, 2L)
  )
})

test_that("third_person counts words, not whole-document matches (#316)", {
  expect_equal(
    third_person(c("They took their books.", "Those are theirs.")),
    c(2L, 2L)
  )
})

test_that("to_be counts words, not whole-document matches (#316)", {
  expect_equal(
    to_be(c("This is a test string", "we are the champions")),
    c(1L, 1L)
  )
})

test_that("prepositions counts words, not whole-document matches (#316)", {
  expect_equal(
    prepositions(c("The cat sat on the mat.", "Look under the table.")),
    c(1L, 1L)
  )
})

test_that("n_uq_urls counts distinct full urls, not just the scheme (#329)", {
  expect_equal(
    n_uq_urls(c(
      "visit https://a.com and https://b.com",
      "only https://a.com",
      "https://a.com and https://a.com again"
    )),
    c(2L, 1L, 1L)
  )
})

test_that("n_charS excludes urls, hashtags, and mentions (#329)", {
  expect_equal(
    n_charS("hi https://example.com #tag @mention bye"),
    nchar("hibye")
  )
  expect_equal(n_charS("hi bye"), nchar("hibye"))
})

test_that("n_uq_charS excludes urls, hashtags, and mentions (#329)", {
  expect_equal(
    n_uq_charS("aa https://example.com #tag @mention bb"),
    dplyr::n_distinct(strsplit("aabb", "")[[1]])
  )
})

test_that("n_extraspaces only counts 2+ consecutive whitespace characters (#329)", {
  expect_equal(n_extraspaces("a\tb"), 0L)
  expect_equal(n_extraspaces("a\nb"), 0L)
  expect_equal(n_extraspaces("a b"), 0L)
  expect_equal(n_extraspaces("a  b"), 1L)
  expect_equal(n_extraspaces("a\t\tb"), 1L)
  expect_equal(n_extraspaces("a   b   c"), 2L)
})

test_that("count functions consistently propagate NA input to NA output (#329)", {
  x <- c("hello world", NA, "test")

  for (fun_name in names(count_functions)) {
    fun <- count_functions[[fun_name]]
    result <- fun(x)
    expect_true(
      is.na(result[2]),
      info = paste(fun_name, "did not return NA for NA input")
    )
    expect_false(anyNA(result[c(1, 3)]))
  }
})
