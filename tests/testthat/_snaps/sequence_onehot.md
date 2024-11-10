# padding and truncating works correctly

    Code
      rec %>% step_tokenize(text) %>% step_sequence_onehot(text, padding = "not pre")
    Condition
      Error in `step_sequence_onehot()`:
      ! `padding` must be one of "pre" or "post", not "not pre".

---

    Code
      rec %>% step_tokenize(text) %>% step_sequence_onehot(text, truncating = "Wrong")
    Condition
      Error in `step_sequence_onehot()`:
      ! `truncating` must be one of "pre" or "post", not "Wrong".

---

    Code
      rec %>% step_tokenize(text) %>% step_sequence_onehot(text, padding = c("pre",
        "pre"))
    Condition
      Error in `step_sequence_onehot()`:
      ! `arg` must be length 1 or a permutation of `c("pre", "post")`.

---

    Code
      rec %>% step_tokenize(text) %>% step_sequence_onehot(text, truncating = "Wrong")
    Condition
      Error in `step_sequence_onehot()`:
      ! `truncating` must be one of "pre" or "post", not "Wrong".

# check_name() is used

    Code
      prep(rec, training = dat)
    Condition
      Error in `step_sequence_onehot()`:
      Caused by error in `bake()`:
      ! Name collision occurred. The following variable names already exist:
      * `seq1hot_text_1`

# bad args

    Code
      recipe(~., data = mtcars) %>% step_sequence_onehot(padding = "yes")
    Condition
      Error in `step_sequence_onehot()`:
      ! `padding` must be one of "pre" or "post", not "yes".

---

    Code
      recipe(~., data = mtcars) %>% step_sequence_onehot(truncating = "yes")
    Condition
      Error in `step_sequence_onehot()`:
      ! `truncating` must be one of "pre" or "post", not "yes".

---

    Code
      recipe(~., data = mtcars) %>% step_sequence_onehot(sequence_length = -4) %>%
        prep()
    Condition
      Error in `step_sequence_onehot()`:
      Caused by error in `prep()`:
      ! `sequence_length` must be a whole number larger than or equal to 0, not the number -4.

---

    Code
      recipe(~., data = mtcars) %>% step_sequence_onehot(prefix = NULL) %>% prep()
    Condition
      Error in `step_sequence_onehot()`:
      Caused by error in `prep()`:
      ! `prefix` must be a single string, not `NULL`.

# bake method errors when needed non-standard role columns are missing

    Code
      bake(trained, new_data = tokenized_test_data[, -1])
    Condition
      Error in `step_sequence_onehot()`:
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
      * Sequence 1 hot encoding for: <none>

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
      * Sequence 1 hot encoding for: <none> | Trained

# keep_original_cols - can prep recipes with it missing

    Code
      rec <- prep(rec)
    Condition
      Warning:
      `keep_original_cols` was added to `step_sequence_onehot()` after this recipe was created.
      i Regenerate your recipe to avoid this warning.

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
      * Sequence 1 hot encoding for: text

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
      * Sequence 1 hot encoding for: text | Trained

