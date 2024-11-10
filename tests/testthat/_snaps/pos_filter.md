# lemmatization errors if lemma attribute doesn't exists

    Code
      prep(rec)
    Condition
      Error in `step_pos_filter()`:
      Caused by error in `bake()`:
      ! `text` doesn't have a pos attribute.
      i Make sure the tokenization step includes part of speech tagging.

# bad args

    Code
      recipe(~., data = mtcars) %>% step_pos_filter(keep_tags = -4) %>% prep()
    Condition
      Error in `step_pos_filter()`:
      Caused by error in `prep()`:
      ! `keep_tags` must be a character vector, not the number -4.

# bake method errors when needed non-standard role columns are missing

    Code
      bake(trained, new_data = tokenized_test_data[, -1])
    Condition
      Error in `step_pos_filter()`:
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
      * Part of speech filtering for: <none>

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
      * Part of speech filtering for: <none> | Trained

# printing

    Code
      print(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 1
      
      -- Operations 
      * Tokenization for: all_predictors()
      * Part of speech filtering for: all_predictors()

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
      * Part of speech filtering for: text | Trained

