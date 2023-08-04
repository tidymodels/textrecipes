# check_name() is used

    Code
      prep(rec, training = dat)
    Error <recipes_error_step>
      Error in `step_tfidf()`:
      Caused by error in `bake()`:
      ! Name collision occured. The following variable names already exists:
      i  tfidf_text_i

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
    Message <cliMessage>
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:    1
      predictor: 10
      
      -- Operations 
      * Term frequency-inverse document frequency with: <none>

---

    Code
      rec
    Message <cliMessage>
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:    1
      predictor: 10
      
      -- Training information 
      Training data contained 32 data points and no incomplete rows.
      
      -- Operations 
      * Term frequency-inverse document frequency with: <none> | Trained

# keep_original_cols - can prep recipes with it missing

    Code
      rec <- prep(rec)
    Warning <rlang_warning>
      'keep_original_cols' was added to `step_tfidf()` after this recipe was created.
      Regenerate your recipe to avoid this warning.

# printing

    Code
      print(rec)
    Message <cliMessage>
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 1
      
      -- Operations 
      * Tokenization for: text
      * Term frequency-inverse document frequency with: text

---

    Code
      prep(rec)
    Message <cliMessage>
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 1
      
      -- Training information 
      Training data contained 4 data points and no incomplete rows.
      
      -- Operations 
      * Tokenization for: text | Trained
      * Term frequency-inverse document frequency with: text | Trained

