# Errors if vocabulary size is set to low.

    Code
      prep(step_tokenize_sentencepiece(recipe(~text1, data = test_data), text1,
      vocabulary_size = 10))
    Condition
      Error in `step_tokenize_sentencepiece()`:
      Caused by error in `prep()`:
      ! The `vocabulary_size` of 10 is too small for column `text1` which has a unique character count of 23.

# bad args

    Code
      prep(step_tokenize_sentencepiece(recipe(~., data = mtcars), vocabulary_size = -
      4))
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

# options are checked (#291)

    Code
      prep(step_tokenize_sentencepiece(rec, text1, options = list(1)))
    Condition
      Error in `step_tokenize_sentencepiece()`:
      Caused by error in `prep()`:
      ! The list passed to `options` must be named.

# `model` is checked (#278)

    Code
      prep(step_tokenize_sentencepiece(rec, text1, model = 1))
    Condition
      Error in `step_tokenize_sentencepiece()`:
      Caused by error in `prep()`:
      ! `model` must be a single string, not the number 1.

---

    Code
      prep(step_tokenize_sentencepiece(rec, text1, model = "does-not-exist.model"))
    Condition
      Error in `step_tokenize_sentencepiece()`:
      Caused by error in `prep()`:
      ! The file 'does-not-exist.model' passed to `model` does not exist.

# pretrained `model` works with factor columns and missing columns

    Code
      bake(rec, new_data = data[, 0])
    Condition
      Error in `bake()`:
      x The following required columns are missing from `new_data`: `text1`.
      i These columns have one of the following roles, which are required at `bake()` time: `predictor`.

