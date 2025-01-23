# ngram works with varrying number of `n`

    Code
      ngram(test_data, n = 0L, n_min = 0L, delim = "_")
    Condition
      Error in `ngram()`:
      ! n must be a positive integer.

---

    Code
      ngram(test_data, n = -1L, n_min = -1L, delim = "_")
    Condition
      Error in `ngram()`:
      ! n must be a positive integer.

# bad args

    Code
      recipe(~., data = mtcars) %>% step_ngram(num_tokens = -4) %>% prep()
    Condition
      Error in `step_ngram()`:
      Caused by error in `prep()`:
      ! `num_tokens` must be a whole number larger than or equal to 0, not the number -4.

---

    Code
      recipe(~., data = mtcars) %>% step_ngram(min_num_tokens = -4) %>% prep()
    Condition
      Error in `step_ngram()`:
      Caused by error in `prep()`:
      ! `min_num_tokens` must be a whole number larger than or equal to 0, not the number -4.

---

    Code
      recipe(~., data = mtcars) %>% step_ngram(delim = -4) %>% prep()
    Condition
      Error in `step_ngram()`:
      Caused by error in `prep()`:
      ! `delim` must be a single string, not the number -4.

# bake method errors when needed non-standard role columns are missing

    Code
      bake(trained, new_data = tokenized_test_data[, -1])
    Condition
      Error in `step_ngram()`:
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
      * ngramming for: <none>

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
      * ngramming for: <none> | Trained

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
      * ngramming for: text

---

    Code
      prep(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 1
      
      -- Training information 
      Training data contained 2 data points and no incomplete rows.
      
      -- Operations 
      * Tokenization for: text | Trained
      * ngramming for: text | Trained

