# step throws an error if unavaliable tokenizer is picked

    Code
      rec %>% step_tokenize(text, token = "wrong") %>% prep()
    Error <rlang_error>
      token should be one of the supported 'characters', token should be one of the supported 'character_shingle', token should be one of the supported 'lines', token should be one of the supported 'ngrams', token should be one of the supported 'paragraphs', token should be one of the supported 'ptb', token should be one of the supported 'regex', token should be one of the supported 'sentences', token should be one of the supported 'skip_ngrams', token should be one of the supported 'tweets', token should be one of the supported 'words', token should be one of the supported 'word_stems'

# tokenization errors with wrong engines

    Code
      rec %>% step_tokenize(text, engine = "fake") %>% prep()
    Error <rlang_error>
      `engine` argument is not valid.

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

---

    Code
      prep(rec, verbose = TRUE)
    Output
      oper 1 step tokenize [training] 
      The retained training set is ~ 0 Mb  in memory.
      
      Recipe
      
      Inputs:
      
            role #variables
       predictor          1
      
      Training data contained 4 data points and no missing data.
      
      Operations:
      
      Tokenization for text [trained]

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
      
      Tokenization for <none>

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
      
      Tokenization for <none> [trained]

