hash_tokens <- list(
  c("a", "ab", "abc", "abcd", "abcde"),
  c("the", "quick", "the", "fox", "the"),
  c("café", "naïve", "日本語", "\U0001F600"),
  character(),
  c("", "x"),
  c(strrep("long", 10), "hello world", "0123456789")
)

# Non-zero entries as "row:col:value" in column-major order
hash_triplets <- function(x, n, signed) {
  res <- list_to_hash(x, n, signed, sparse = "no")
  idx <- which(res != 0, arr.ind = TRUE)
  idx <- idx[order(idx[, 2], idx[, 1]), , drop = FALSE]
  paste(idx[, 1], idx[, 2], res[idx], sep = ":")
}

# The expected values below were generated with text2vec::hash_vectorizer()
test_that("hashing reproduces known text2vec output (unsigned)", {
  expect_identical(
    hash_triplets(hash_tokens, 8, FALSE),
    c(
      "3:1:1", "5:1:1", "2:2:1", "1:3:1", "3:3:1", "1:4:2", "2:4:1", "1:6:1",
      "2:6:3", "3:6:2", "6:6:2", "1:7:1", "5:8:1", "6:8:1"
    )
  )
  expect_identical(
    hash_triplets(hash_tokens, 100, FALSE),
    c(
      "3:1:1", "3:2:1", "3:11:1", "5:20:1", "2:36:1", "3:54:1", "2:58:1",
      "6:58:1", "6:62:1", "1:63:1", "6:64:1", "5:69:1", "1:70:1", "2:70:3",
      "1:75:1", "1:76:1", "1:80:1"
    )
  )
})

test_that("hashing reproduces known text2vec output (signed)", {
  expect_identical(
    hash_triplets(hash_tokens, 8, TRUE),
    c(
      "3:1:-1", "5:1:-1", "2:2:1", "1:3:1", "3:3:-1", "1:4:2", "2:4:1",
      "1:6:-1", "2:6:3", "3:6:-2", "6:6:-2", "1:7:1", "5:8:-1", "6:8:1"
    )
  )
  expect_identical(
    hash_triplets(hash_tokens, 1024, TRUE),
    c(
      "3:43:-1", "6:128:1", "3:238:-1", "2:292:1", "2:374:3", "3:430:-1",
      "5:464:-1", "3:569:-1", "6:614:-1", "2:626:1", "5:729:-1", "1:796:1",
      "1:814:-1", "6:830:-1", "1:839:1", "1:868:1", "1:915:1"
    )
  )
})

test_that("hashing handles zero documents", {
  res <- list_to_hash(list(), 8, TRUE, sparse = "no")
  expect_identical(dim(res), c(0L, 8L))
})

test_that("hashing handles documents without tokens", {
  res <- list_to_hash(list(character(), character()), 8, FALSE, sparse = "no")
  expect_identical(dim(res), c(2L, 8L))
  expect_true(all(res == 0))
})

test_that("hashing matches text2vec::hash_vectorizer()", {
  skip_if_not_installed("text2vec")

  for (signed in c(FALSE, TRUE)) {
    for (n in c(8, 100, 1024)) {
      vectorizer <- text2vec::hash_vectorizer(
        hash_size = n,
        signed_hash = signed
      )
      it <- text2vec::itoken(hash_tokens, progress = FALSE)
      expected <- as.matrix(text2vec::create_dtm(it, vectorizer))

      expect_equal(
        unname(list_to_hash(hash_tokens, n, signed, sparse = "no")),
        unname(expected)
      )
    }
  }
})

test_that("split_on_space() matches the text2vec space tokenizer", {
  skip_if_not_installed("text2vec")
  skip_if_not_installed("stringi")

  x <- c("", "a", "a b", "a ", " a", "a  b", "café naïve", "x y z ")

  expect_identical(
    split_on_space(x),
    stringi::stri_split_fixed(x, " ")
  )
})
