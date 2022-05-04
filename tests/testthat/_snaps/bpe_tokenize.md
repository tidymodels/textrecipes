# Errors if vocabulary size is set to low.

    Code
      recipe(~text1, data = test_data) %>% step_tokenize_bpe(text1, vocabulary_size = 10) %>%
        prep()
    Error <rlang_error>
      `vocabulary_size` of 10 is too small for column `text1` which has a unique character count of 23

# printing

    Code
      print(rec)
    Output
      Recipe
      
      Inputs:
      
            role #variables
       predictor          2
      
      Operations:
      
      BPE Tokenization for text1

---

    Code
      prep(rec)
    Output
      Recipe
      
      Inputs:
      
            role #variables
       predictor          2
      
      Training data contained 4 data points and no missing data.
      
      Operations:
      
      BPE Tokenization for text1 [trained]

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
      
      BPE Tokenization for <none>

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
      
      BPE Tokenization for <none> [trained]

