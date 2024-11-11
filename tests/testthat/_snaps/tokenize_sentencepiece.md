# Errors if vocabulary size is set to low.

    Code
      recipe(~text1, data = test_data) %>% step_tokenize_sentencepiece(text1,
        vocabulary_size = 10) %>% prep()
    Condition
      Error in `step_tokenize_sentencepiece()`:
      Caused by error in `prep()`:
      ! The `vocabulary_size` of 10 is too small for column `text1` which has a unique character count of 23.

# bad args

    Code
      recipe(~., data = mtcars) %>% step_tokenize_sentencepiece(vocabulary_size = -4) %>%
        prep()
    Condition
      Error in `step_tokenize_sentencepiece()`:
      Caused by error in `prep()`:
      ! `vocabulary_size` must be a whole number larger than or equal to 0, not the number -4.

# bake method errors when needed non-standard role columns are missing

    Code
      bake(trained, new_data = test_data[, -1])
    Condition
      Error in `step_tokenize_sentencepiece()`:
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
      * Sentencepiece Tokenization for: <none>

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
      * Sentencepiece Tokenization for: <none> | Trained

# printing

    Code
      print(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 2
      
      -- Operations 
      * Sentencepiece Tokenization for: text1

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
      * Sentencepiece Tokenization for: text1 | Trained

