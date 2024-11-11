# tokenfilter removes words correctly using min_times and max_times

    Code
      obj <- rec %>% prep()
    Condition
      Warning:
      max_tokens was set to 100, but only 3 was available and selected.

# removes words correctly with min_times, max_times and procentage

    Code
      obj <- rec %>% prep()
    Condition
      Warning:
      max_tokens was set to 100, but only 12 was available and selected.

# tokenfilter throws warning when max_tokens > words

    Code
      rec %>% prep()
    Condition
      Warning:
      max_tokens was set to 10000, but only 17 was available and selected.
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 1
      
      -- Training information 
      Training data contained 4 data points and no incomplete rows.
      
      -- Operations 
      * Tokenization for: text | Trained
      * Text filtering for: text | Trained

# bad args

    Code
      recipe(~., data = mtcars) %>% step_tokenfilter(percentage = "yes") %>% prep()
    Condition
      Error in `step_tokenfilter()`:
      Caused by error in `prep()`:
      ! `percentage` must be `TRUE` or `FALSE`, not the string "yes".

---

    Code
      recipe(~., data = mtcars) %>% step_tokenfilter(max_tokens = -4) %>% prep()
    Condition
      Error in `step_tokenfilter()`:
      Caused by error in `prep()`:
      ! `max_tokens` must be a whole number larger than or equal to 0, not the number -4.

---

    Code
      recipe(~., data = mtcars) %>% step_tokenfilter(filter_fun = -4) %>% prep()
    Condition
      Error in `step_tokenfilter()`:
      Caused by error in `prep()`:
      ! `filter_fun` must be a function or `NULL`, not the number -4.

---

    Code
      recipe(~., data = mtcars) %>% step_tokenfilter(percentage = TRUE, max_times = 2) %>%
        prep()
    Condition
      Error in `step_tokenfilter()`:
      Caused by error in `prep()`:
      ! `max_times` must be a number between 0 and 1, not the number 2.

---

    Code
      recipe(~., data = mtcars) %>% step_tokenfilter(percentage = TRUE, min_times = 2) %>%
        prep()
    Condition
      Error in `step_tokenfilter()`:
      Caused by error in `prep()`:
      ! `min_times` must be a number between 0 and 1, not the number 2.

---

    Code
      recipe(~., data = mtcars) %>% step_tokenfilter(percentage = FALSE, max_times = -
        1) %>% prep()
    Condition
      Error in `step_tokenfilter()`:
      Caused by error in `prep()`:
      ! `max_times` must be a whole number larger than or equal to 0, not the number -1.

---

    Code
      recipe(~., data = mtcars) %>% step_tokenfilter(percentage = FALSE, min_times = -
        1) %>% prep()
    Condition
      Error in `step_tokenfilter()`:
      Caused by error in `prep()`:
      ! `min_times` must be a whole number larger than or equal to 0, not the number -1.

# bake method errors when needed non-standard role columns are missing

    Code
      bake(trained, new_data = tokenized_test_data[, -1])
    Condition
      Error in `step_tokenfilter()`:
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
      * Text filtering for: <none>

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
      * Text filtering for: <none> | Trained

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
      * Text filtering for: text

---

    Code
      prep(rec)
    Condition
      Warning:
      max_tokens was set to 100, but only 17 was available and selected.
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 1
      
      -- Training information 
      Training data contained 4 data points and no incomplete rows.
      
      -- Operations 
      * Tokenization for: text | Trained
      * Text filtering for: text | Trained

