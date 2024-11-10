# check_name() is used

    Code
      prep(rec, training = dat)
    Condition
      Error in `step_dummy_hash()`:
      Caused by error in `bake()`:
      ! Name collision occurred. The following variable names already exist:
      * `dummyhash_text_01`

# bad args

    Code
      recipe(~., data = mtcars) %>% step_dummy_hash(signed = "yes") %>% prep()
    Condition
      Error in `step_dummy_hash()`:
      Caused by error in `prep()`:
      ! `signed` must be `TRUE` or `FALSE`, not the string "yes".

---

    Code
      recipe(~., data = mtcars) %>% step_dummy_hash(num_terms = -4) %>% prep()
    Condition
      Error in `step_dummy_hash()`:
      Caused by error in `prep()`:
      ! `num_terms` must be a whole number larger than or equal to 0, not the number -4.

---

    Code
      recipe(~., data = mtcars) %>% step_dummy_hash(collapse = "yes") %>% prep()
    Condition
      Error in `step_dummy_hash()`:
      Caused by error in `prep()`:
      ! `collapse` must be `TRUE` or `FALSE`, not the string "yes".

# bake method errors when needed non-standard role columns are missing

    Code
      bake(trained, new_data = test_data[, -2])
    Condition
      Error in `step_dummy_hash()`:
      ! The following required column is missing from `new_data`: sponsor_code.

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
      `keep_original_cols` was added to `step_dummy_hash()` after this recipe was created.
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
      * Feature hashing with: Species

---

    Code
      prep(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 5
      
      -- Training information 
      Training data contained 150 data points and no incomplete rows.
      
      -- Operations 
      * Feature hashing with: Species | Trained

