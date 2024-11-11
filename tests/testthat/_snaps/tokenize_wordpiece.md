# bad args

    Code
      recipe(~., data = mtcars) %>% step_tokenize_wordpiece(unk_token = 0) %>% prep()
    Condition
      Error in `step_tokenize_wordpiece()`:
      Caused by error in `prep()`:
      ! `unk_token` must be a single string, not the number 0.

---

    Code
      recipe(~., data = mtcars) %>% step_tokenize_wordpiece(max_chars = -4) %>% prep()
    Condition
      Error in `step_tokenize_wordpiece()`:
      Caused by error in `prep()`:
      ! `max_chars` must be a whole number larger than or equal to 0, not the number -4.

# bake method errors when needed non-standard role columns are missing

    Code
      bake(trained, new_data = test_data[, -1])
    Condition
      Error in `step_tokenize_wordpiece()`:
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
      * wordpiece Tokenization for: <none>

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
      * wordpiece Tokenization for: <none> | Trained

# printing

    Code
      print(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 2
      
      -- Operations 
      * wordpiece Tokenization for: text1

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
      * wordpiece Tokenization for: text1 | Trained

