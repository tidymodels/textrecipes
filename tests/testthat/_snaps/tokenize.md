# step throws an error if unavaliable tokenizer is picked

    Code
      prep(step_tokenize(rec, text, token = "wrong"))
    Condition
      Error in `step_tokenize()`:
      Caused by error in `prep()`:
      ! Token should be one of characters, character_shingle, lines, ngrams, paragraphs, ptb, regex, sentences, skip_ngrams, words, and word_stems.

# tokenization errors with wrong engines

    Code
      prep(step_tokenize(rec, text, engine = "fake"))
    Condition
      Error in `step_tokenize()`:
      Caused by error in `prep()`:
      ! The `engine` argument is not valid.

# bad args

    Code
      prep(step_tokenize(recipe(~., data = mtcars), token = letters))
    Condition
      Error in `step_tokenize()`:
      Caused by error in `prep()`:
      ! `token` must be a single string, not a character vector.

---

    Code
      prep(step_tokenize(recipe(~., data = mtcars), engine = letters))
    Condition
      Error in `step_tokenize()`:
      Caused by error in `prep()`:
      ! `engine` must be a single string, not a character vector.

---

    Code
      prep(step_tokenize(recipe(~., data = mtcars), custom_token = "yes"))
    Condition
      Error in `step_tokenize()`:
      Caused by error in `prep()`:
      ! `custom_token` must be a function or `NULL`, not the string "yes".

# bake method errors when needed non-standard role columns are missing

    Code
      bake(trained, new_data = test_data[, -1])
    Condition
      Error in `step_tokenize()`:
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
      * Tokenization for: <none>

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
      * Tokenization for: <none> | Trained

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

