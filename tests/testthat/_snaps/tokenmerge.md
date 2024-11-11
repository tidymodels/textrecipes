# it complains when the selected column isn't a tokenlist

    Code
      prep(rec)
    Condition
      Error in `step_tokenmerge()`:
      Caused by error in `prep()`:
      x All columns selected for the step should be tokenlist.
      * 2 factor variables found: `text1` and `text2`

# check_name() is used

    Code
      prep(rec, training = dat)
    Condition
      Error in `step_tokenmerge()`:
      Caused by error in `bake()`:
      ! Name collision occurred. The following variable names already exist:
      * `tokenmerge`

# bad args

    Code
      recipe(~., data = mtcars) %>% step_tokenmerge(prefix = NULL) %>% prep()
    Condition
      Error in `step_tokenmerge()`:
      Caused by error in `prep()`:
      ! `prefix` must be a single string, not `NULL`.

# bake method errors when needed non-standard role columns are missing

    Code
      bake(trained, new_data = tokenized_test_data[, -1])
    Condition
      Error in `step_tokenmerge()`:
      ! The following required column is missing from `new_data`: text1.

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
      * Merging tokens for: <none>

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
      * Merging tokens for: <none> | Trained

# keep_original_cols - can prep recipes with it missing

    Code
      rec <- prep(rec)
    Condition
      Warning:
      `keep_original_cols` was added to `step_tokenmerge()` after this recipe was created.
      i Regenerate your recipe to avoid this warning.

# printing

    Code
      print(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 2
      
      -- Operations 
      * Tokenization for: text1 and text2
      * Merging tokens for: text1 and text2

---

    Code
      prep(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 2
      
      -- Training information 
      Training data contained 4 data points and no incomplete rows.
      
      -- Operations 
      * Tokenization for: text1 and text2 | Trained
      * Merging tokens for: text1 and text2 | Trained

