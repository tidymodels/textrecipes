# printing

    Code
      print(rec)
    Output
      Recipe
      
      Inputs:
      
            role #variables
       predictor          1
      
      Operations:
      
      Tokenization for text
      Term frequency-inverse document frequency with text

# can prep recipes with no keep_original_cols

    Code
      koc_trained <- prep(koc_rec, training = test_data, verbose = FALSE)
    Warning <rlang_warning>
      'keep_original_cols' was added to `step_tfidf()` after this recipe was created.
      Regenerate your recipe to avoid this warning.

# Backwards compatibility with 1592690d36581fc5f4952da3e9b02351b31f1a2e

    Code
      expect_equal(bake(rec, data) %>% slice(1), tibble(tfidf_text_g = log(1 + 2 / 1) /
        2, tfidf_text_i = log(1 + 2 / 2) / 2))
    Warning <rlang_warning>
      Please retrain this recipe with version 0.5.1 or higher.
      * A data leakage bug has been fixed for `step_tfidf()`.

---

    Code
      expect_equal(bake(rec, data %>% slice(1)), tibble(tfidf_text_g = log(1 + 2 / 2) /
        2, tfidf_text_i = log(1 + 2 / 2) / 2))
    Warning <rlang_warning>
      Please retrain this recipe with version 0.5.1 or higher.
      * A data leakage bug has been fixed for `step_tfidf()`.

# empty printing

    Code
      rec
    Output
      Recipe
      
      Inputs:
      
            role #variables
         outcome          1
       predictor         10
      
      Operations:
      
      Term frequency-inverse document frequency with <none>

---

    Code
      rec
    Output
      Recipe
      
      Inputs:
      
            role #variables
         outcome          1
       predictor         10
      
      Training data contained 32 data points and no missing data.
      
      Operations:
      
      Term frequency-inverse document frequency with <none> [trained]

