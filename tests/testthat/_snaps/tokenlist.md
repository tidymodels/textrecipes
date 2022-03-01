# tokenlist errors with diffent length input

    Code
      tokenlist(list(letters), lemma = list(letters, letters))
    Error <rlang_error>
      All fields must be the same size.

---

    Code
      tokenlist(list(letters), pos = list(letters, letters))
    Error <rlang_error>
      All fields must be the same size.

# new_tokenlist errors with wrong input

    Code
      new_tokenlist(letters)
    Error <vctrs_error_assert_ptype>
      `tokens` must be a vector with type <list>.
      Instead, it has type <character>.

---

    Code
      new_tokenlist(list(letters), lemma = letters)
    Error <rlang_error>
      `lemma` must be NULL or a list.

---

    Code
      new_tokenlist(list(letters), pos = letters)
    Error <rlang_error>
      `pos` must be NULL or a list.

# subsetting works as intended

    Code
      tokenlist(data)[3]
    Error <vctrs_error_subscript_oob>
      Can't subset elements that don't exist.
      x Location 3 doesn't exist.
      i There are only 2 elements.

# tokenlist_filter works

    Code
      tokenlist_filter(LETTERS, letters)
    Error <rlang_error>
      Input must be a tokenlist.

# tokenlist_apply works

    Code
      tokenlist_apply(tkn_list, letter_filter, let = "D")
    Error <simpleError>
      unused argument (let = "D")

---

    Code
      tokenlist_apply(letters, toupper)
    Error <rlang_error>
      Input must be a tokenlist.

# tokenlist_to_dtm works

    Code
      tokenlist_to_dtm(tkn_list)
    Error <simpleError>
      argument "dict" is missing, with no default

---

    Code
      tokenlist_to_dtm(letters)
    Error <rlang_error>
      Input must be a tokenlist.

# tokenlist_lemma works

    Code
      tokenlist_lemma(letters)
    Error <rlang_error>
      Input must be a tokenlist.

---

    Code
      tokenlist_lemma(tokenlist(list(letters)))
    Error <rlang_error>
      `lemma` attribute not avaliable.

# tokenlist_pos_filter works

    Code
      tokenlist_pos_filter(letters, "NOUN")
    Error <rlang_error>
      Input must be a tokenlist.

---

    Code
      tokenlist_pos_filter(tokenlist(data), "NOUN")
    Error <rlang_error>
      pos attribute not avaliable.

# tokenlist_ngram errors

    Code
      tokenlist_ngram(letters)
    Error <rlang_error>
      Input must be a tokenlist.

---

    Code
      tokenlist_ngram(tokenlist(data), 0, 3, " ")
    Error <Rcpp::exception>
      'n' must be a positive integer.

---

    Code
      tokenlist_ngram(tokenlist(data), 3, 0, " ")
    Error <Rcpp::exception>
      'n_min' must be a positive integer.

---

    Code
      tokenlist_ngram(tokenlist(data), 1, 2, " ")
    Error <Rcpp::exception>
      'n_min' must be larger then 'n'.

