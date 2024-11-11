# check_name() is used

    Code
      prep(rec, training = dat)
    Condition
      Error in `step_lda()`:
      Caused by error in `bake()`:
      ! Name collision occurred. The following variable names already exist:
      * `lda_text_1`

# bad args

    Code
      recipe(~., data = mtcars) %>% step_lda(num_topics = -4) %>% prep()
    Condition
      Error in `step_lda()`:
      Caused by error in `prep()`:
      ! `num_topics` must be a whole number larger than or equal to 0, not the number -4.

---

    Code
      recipe(~., data = mtcars) %>% step_lda(prefix = NULL) %>% prep()
    Condition
      Error in `step_lda()`:
      Caused by error in `prep()`:
      ! `prefix` must be a single string, not `NULL`.

# bake method errors when needed non-standard role columns are missing

    Code
      bake(trained, new_data = tokenized_test_data[, -1])
    Condition
      Error in `step_lda()`:
      ! The following required column is missing from `new_data`: medium.

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
      * Text feature extraction for: <none>

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
      * Text feature extraction for: <none> | Trained

# keep_original_cols - can prep recipes with it missing

    Code
      rec <- prep(rec)
    Condition
      Warning:
      `keep_original_cols` was added to `step_lda()` after this recipe was created.
      i Regenerate your recipe to avoid this warning.

# printing

    Code
      print(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 5
      
      -- Operations 
      * Tokenization for: Species
      * Text feature extraction for: Species

---

    Code
      prep(rec)
    Condition
      Warning in `get_dtm()`:
      dtm has 0 rows. Empty iterator?
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 5
      
      -- Training information 
      Training data contained 150 data points and no incomplete rows.
      
      -- Operations 
      * Tokenization for: Species | Trained
      * Text feature extraction for: Species | Trained

