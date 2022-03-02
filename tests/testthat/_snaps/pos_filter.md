# lemmatization errors if lemma attribute doesn't exists

    Code
      prep(rec)
    Error <rlang_error>
      `text` doesn't have a pos attribute. Make sure the tokenization step includes part of speech tagging.

# printing

    Code
      print(rec)
    Output
      Recipe
      
      Inputs:
      
            role #variables
       predictor          1
      
      Operations:
      
      Tokenization for all_predictors()
      Part of speech filtering for all_predictors()

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
      
      Part of speech filtering for <none>

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
      
      Part of speech filtering for <none> [trained]

