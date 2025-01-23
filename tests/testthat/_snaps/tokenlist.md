# tokenlist errors with diffent length input

    Code
      tokenlist(list(letters), lemma = list(letters, letters))
    Condition
      Error in `vctrs::new_rcrd()`:
      ! All fields must be the same size.

---

    Code
      tokenlist(list(letters), pos = list(letters, letters))
    Condition
      Error in `vctrs::new_rcrd()`:
      ! All fields must be the same size.

# new_tokenlist errors with wrong input

    Code
      new_tokenlist(letters)
    Condition
      Error in `new_tokenlist()`:
      ! `tokens` must be a vector with type <list>.
      Instead, it has type <character>.

---

    Code
      new_tokenlist(list(letters), lemma = letters)
    Condition
      Error in `new_tokenlist()`:
      ! `lemma` must be NULL or a list.

---

    Code
      new_tokenlist(list(letters), pos = letters)
    Condition
      Error in `new_tokenlist()`:
      ! `pos` must be NULL or a list.

# Printing works

    Code
      tokenlist(list(letters, letters))
    Output
      <textrecipes_tokenlist[2]>
      [1] [26 tokens] [26 tokens]
      # Unique Tokens: 26

# subsetting works as intended

    Code
      tokenlist(data)[3]
    Condition
      Error in `vec_slice()`:
      ! Can't subset elements past the end.
      i Location 3 doesn't exist.
      i There are only 2 elements.

# tokenlist_filter works

    Code
      tokenlist_filter(LETTERS, letters)
    Condition
      Error in `tokenlist_filter()`:
      ! Input must be a tokenlist.

# tokenlist_apply works

    Code
      tokenlist_apply(tkn_list, letter_filter, let = "D")
    Condition
      Error in `tokenlist_apply()`:
      ! unused argument (let = "D")

---

    Code
      tokenlist_apply(letters, toupper)
    Condition
      Error in `tokenlist_apply()`:
      ! Input must be <tokenlist> object.

# tokenlist_lemma works

    Code
      tokenlist_lemma(letters)
    Condition
      Error in `tokenlist_lemma()`:
      ! Input must be a tokenlist.

---

    Code
      tokenlist_lemma(tokenlist(list(letters)))
    Condition
      Error in `tokenlist_lemma()`:
      ! The `lemma` attribute is not available.

# tokenlist_pos_filter works

    Code
      tokenlist_pos_filter(letters, "NOUN")
    Condition
      Error in `tokenlist_pos_filter()`:
      ! Input must be a tokenlist.

---

    Code
      tokenlist_pos_filter(tokenlist(data), "NOUN")
    Condition
      Error in `tokenlist_pos_filter()`:
      ! `pos` attribute not available.

# tokenlist_ngram errors

    Code
      tokenlist_ngram(letters)
    Condition
      Error in `tokenlist_ngram()`:
      ! Input must be a tokenlist.

---

    Code
      tokenlist_ngram(tokenlist(data), 0, 3, " ")
    Condition
      Error in `ngram()`:
      ! n must be a positive integer.

---

    Code
      tokenlist_ngram(tokenlist(data), 3, 0, " ")
    Condition
      Error in `ngram()`:
      ! n_min must be a positive integer.

---

    Code
      tokenlist_ngram(tokenlist(data), 1, 2, " ")
    Condition
      Error in `ngram()`:
      ! n_min must be less then n.

