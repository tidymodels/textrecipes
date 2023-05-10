# step throws an error if unavaliable tokenizer is picked

    Code
      rec %>% step_tokenize(text, token = "wrong") %>% prep()
    Error <recipes_error_step>
      Error in `step_tokenize()`:
      Caused by error in `prep()`:
      ! token should be one of the supported: characters, character_shingle, lines, ngrams, paragraphs, ptb, regex, sentences, skip_ngrams, words, or word_stems

# tokenization errors with wrong engines

    Code
      rec %>% step_tokenize(text, engine = "fake") %>% prep()
    Error <recipes_error_step>
      Error in `step_tokenize()`:
      Caused by error in `prep()`:
      ! `engine` argument is not valid.

# empty printing

    Code
      rec
    Message <cliMessage>
      
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
    Message <cliMessage>
      
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
    Message <cliMessage>
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 1
      
      -- Operations 
      * Tokenization for: text

---

    Code
      prep(rec)
    Message <cliMessage>
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 1
      
      -- Training information 
      Training data contained 4 data points and no incomplete rows.
      
      -- Operations 
      * Tokenization for: text | Trained

