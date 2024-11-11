# check_name() is used

    Code
      prep(rec, training = dat)
    Condition
      Error in `step_tfidf()`:
      Caused by error in `bake()`:
      ! Name collision occurred. The following variable names already exist:
      * `tfidf_text_i`

# Backwards compatibility with 1592690d36581fc5f4952da3e9b02351b31f1a2e

    Code
      expect_equal(bake(rec, data) %>% slice(1), tibble(tfidf_text_g = log(1 + 2 / 1) /
        2, tfidf_text_i = log(1 + 2 / 2) / 2))
    Condition
      Warning:
      Please retrain this recipe with version 0.5.1 or higher.
      i A data leakage bug has been fixed for `step_tfidf()`.

---

    Code
      expect_equal(bake(rec, data %>% slice(1)), tibble(tfidf_text_g = log(1 + 2 / 2) /
        2, tfidf_text_i = log(1 + 2 / 2) / 2))
    Condition
      Warning:
      Please retrain this recipe with version 0.5.1 or higher.
      i A data leakage bug has been fixed for `step_tfidf()`.

# bad args

    Code
      recipe(~., data = mtcars) %>% step_tfidf(vocabulary = 1:10) %>% prep()
    Condition
      Error in `step_tfidf()`:
      Caused by error in `prep()`:
      ! `vocabulary` must be a character vector or `NULL`, not an integer vector.

---

    Code
      recipe(~., data = mtcars) %>% step_tfidf(smooth_idf = "yes") %>% prep()
    Condition
      Error in `step_tfidf()`:
      Caused by error in `prep()`:
      ! `smooth_idf` must be `TRUE` or `FALSE`, not the string "yes".

---

    Code
      recipe(~., data = mtcars) %>% step_tfidf(norm = "yes") %>% prep()
    Condition
      Error in `step_tfidf()`:
      Caused by error in `prep()`:
      ! `norm` must be one of "l1", "l2", or "none", not "yes".

---

    Code
      recipe(~., data = mtcars) %>% step_tfidf(sublinear_tf = "yes") %>% prep()
    Condition
      Error in `step_tfidf()`:
      Caused by error in `prep()`:
      ! `sublinear_tf` must be `TRUE` or `FALSE`, not the string "yes".

---

    Code
      recipe(~., data = mtcars) %>% step_tfidf(prefix = NULL) %>% prep()
    Condition
      Error in `step_tfidf()`:
      Caused by error in `prep()`:
      ! `prefix` must be a single string, not `NULL`.

# bake method errors when needed non-standard role columns are missing

    Code
      bake(trained, new_data = tokenized_test_data[, -1])
    Condition
      Error in `step_tfidf()`:
      ! The following required column is missing from `new_data`: text.

# empty printing

    Code
      rec
    Message
      
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
    Message
      
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
    Condition
      Warning:
      `keep_original_cols` was added to `step_tfidf()` after this recipe was created.
      i Regenerate your recipe to avoid this warning.

# printing

    Code
      print(rec)
    Message
      
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
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 1
      
      -- Training information 
      Training data contained 4 data points and no incomplete rows.
      
      -- Operations 
      * Tokenization for: text | Trained
      * Term frequency-inverse document frequency with: text | Trained

