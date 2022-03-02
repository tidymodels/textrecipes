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

