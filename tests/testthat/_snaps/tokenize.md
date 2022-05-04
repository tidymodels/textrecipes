# step throws an error if unavaliable tokenizer is picked

    Code
      rec %>% step_tokenize(text, token = "wrong") %>% prep()
    Error <rlang_error>
      token should be one of the supported: characters, character_shingle, lines, ngrams, paragraphs, ptb, regex, sentences, skip_ngrams, tweets, words, or word_stems

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
      prep(rec)
    Output
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

