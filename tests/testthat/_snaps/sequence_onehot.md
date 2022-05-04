# padding and truncating works correctly

    Code
      rec %>% step_tokenize(text) %>% step_sequence_onehot(text, padding = "not pre")
    Error <rlang_error>
      `padding` should be one of: 'pre', 'post'

---

    Code
      rec %>% step_tokenize(text) %>% step_sequence_onehot(text, truncating = "Wrong")
    Error <rlang_error>
      `truncating` should be one of: 'pre', 'post'

---

    Code
      rec %>% step_tokenize(text) %>% step_sequence_onehot(text, padding = c("pre",
        "pre"))
    Error <rlang_error>
      `padding` should be one of: 'pre', 'post'

---

    Code
      rec %>% step_tokenize(text) %>% step_sequence_onehot(text, truncating = "Wrong")
    Error <rlang_error>
      `truncating` should be one of: 'pre', 'post'

# printing

    Code
      print(rec)
    Output
      Recipe
      
      Inputs:
      
            role #variables
       predictor          1
      
      Operations:
      
      Tokenization for text
      Sequence 1 hot encoding for text

---

    Code
      prep(rec)
    Output
      Recipe
      
      Inputs:
      
            role #variables
       predictor          1
      
      Training data contained 4 data points and no missing data.
      
      Operations:
      
      Tokenization for text [trained]
      Sequence 1 hot encoding for text [trained]

# can prep recipes with no keep_original_cols

    Code
      koc_trained <- prep(koc_rec, training = test_data, verbose = FALSE)
    Warning <rlang_warning>
      'keep_original_cols' was added to `step_sequence_onehot()` after this recipe was created.
      Regenerate your recipe to avoid this warning.

# empty printing

    Code
      rec
    Output
      Recipe
      
      Inputs:
      
            role #variables
         outcome          1
       predictor         10
      
      Operations:
      
      Sequence 1 hot encoding for <none>

---

    Code
      rec
    Output
      Recipe
      
      Inputs:
      
            role #variables
         outcome          1
       predictor         10
      
      Training data contained 32 data points and no missing data.
      
      Operations:
      
      Sequence 1 hot encoding for <none> [trained]

