# check_name() is used

    Code
      prep(rec, training = dat)
    Condition
      Error in `step_tf()`:
      Caused by error in `bake()`:
      ! Name collision occurred. The following variable names already exist:
      * `tf_text_i`

# bad args

    Code
      recipe(~., data = mtcars) %>% step_tf(weight_scheme = "wrong") %>% prep()
    Condition
      Error in `step_tf()`:
      Caused by error in `prep()`:
      ! `weight_scheme` must be one of "binary", "raw count", "term frequency", "log normalization", or "double normalization", not "wrong".

---

    Code
      recipe(~., data = mtcars) %>% step_tf(weight = "wrong") %>% prep()
    Condition
      Error in `step_tf()`:
      Caused by error in `prep()`:
      ! `weight` must be a number, not the string "wrong".

---

    Code
      recipe(~., data = mtcars) %>% step_tf(vocabulary = 1:10) %>% prep()
    Condition
      Error in `step_tf()`:
      Caused by error in `prep()`:
      ! `vocabulary` must be a character vector or `NULL`, not an integer vector.

---

    Code
      recipe(~., data = mtcars) %>% step_tf(prefix = NULL) %>% prep()
    Condition
      Error in `step_tf()`:
      Caused by error in `prep()`:
      ! `prefix` must be a single string, not `NULL`.

# bake method errors when needed non-standard role columns are missing

    Code
      bake(trained, new_data = tokenized_test_data[, -1])
    Condition
      Error in `step_tf()`:
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
      * Term frequency with: <none>

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
      * Term frequency with: <none> | Trained

# keep_original_cols - can prep recipes with it missing

    Code
      rec <- prep(rec)
    Condition
      Warning:
      `keep_original_cols` was added to `step_tf()` after this recipe was created.
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
      * Term frequency with: text

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
      * Term frequency with: text | Trained

