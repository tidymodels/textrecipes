# Errors if vocabulary size is set to low.

    Code
      recipe(~text1, data = test_data) %>% step_tokenize_sentencepiece(text1,
        vocabulary_size = 10) %>% prep()
    Condition
      Error in `step_tokenize_sentencepiece()`:
      Caused by error in `prep()`:
      ! `vocabulary_size` of 10 is too small for column `text1` which has a unique character count of 23.

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

