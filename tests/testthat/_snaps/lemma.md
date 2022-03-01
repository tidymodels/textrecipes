# lemmatization errors if lemma attribute doesn't exists

    Code
      prep(rec)
    Error <rlang_error>
      `text` doesn't have a lemma attribute. Make sure the tokenization step includes lemmatization.

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
      
      Lemmatization for <none>

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
      
      Lemmatization for <none> [trained]

