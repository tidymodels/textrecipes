# bad args

    Code
      recipe(~., data = mtcars) %>% step_stopwords(language = -4) %>% prep()
    Condition
      Error in `step_stopwords()`:
      Caused by error in `prep()`:
      ! `language` must be a single string, not the number -4.

---

    Code
      recipe(~., data = mtcars) %>% step_stopwords(keep = -4) %>% prep()
    Condition
      Error in `step_stopwords()`:
      Caused by error in `prep()`:
      ! `keep` must be `TRUE` or `FALSE`, not the number -4.

---

    Code
      recipe(~., data = mtcars) %>% step_stopwords(stopword_source = -4) %>% prep()
    Condition
      Error in `step_stopwords()`:
      Caused by error in `prep()`:
      ! `stopword_source` must be a single string, not the number -4.

---

    Code
      recipe(~., data = mtcars) %>% step_stopwords(custom_stopword_source = 1:10) %>%
        prep()
    Condition
      Error in `step_stopwords()`:
      Caused by error in `prep()`:
      ! `custom_stopword_source` must be a character vector or `NULL`, not an integer vector.

# bake method errors when needed non-standard role columns are missing

    Code
      bake(trained, new_data = tokenized_test_data[, -1])
    Condition
      Error in `step_stopwords()`:
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
      * Stop word removal for: <none>

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
      * Stop word removal for: <none> | Trained

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
      * Stop word removal for: text

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
      * Stop word removal for: text | Trained

