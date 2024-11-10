# check_name() is used

    Code
      prep(rec, training = dat)
    Condition
      Error in `step_texthash()`:
      Caused by error in `bake()`:
      ! Name collision occurred. The following variable names already exist:
      * `texthash_text_0001`

# bad args

    Code
      recipe(~., data = mtcars) %>% step_texthash(signed = "yes") %>% prep()
    Condition
      Error in `step_texthash()`:
      Caused by error in `prep()`:
      ! `signed` must be `TRUE` or `FALSE`, not the string "yes".

---

    Code
      recipe(~., data = mtcars) %>% step_texthash(num_terms = -4) %>% prep()
    Condition
      Error in `step_texthash()`:
      Caused by error in `prep()`:
      ! `num_terms` must be a whole number larger than or equal to 0, not the number -4.

---

    Code
      recipe(~., data = mtcars) %>% step_texthash(prefix = NULL) %>% prep()
    Condition
      Error in `step_texthash()`:
      Caused by error in `prep()`:
      ! `prefix` must be a single string, not `NULL`.

# bake method errors when needed non-standard role columns are missing

    Code
      bake(trained, new_data = tokenized_test_data[, -1])
    Condition
      Error in `step_texthash()`:
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
      * Feature hashing with: <none>

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
      * Feature hashing with: <none> | Trained

# keep_original_cols - can prep recipes with it missing

    Code
      rec <- prep(rec)
    Condition
      Warning:
      `keep_original_cols` was added to `step_texthash()` after this recipe was created.
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
      * Feature hashing with: text

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
      * Feature hashing with: text | Trained

