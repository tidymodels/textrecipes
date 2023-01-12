# lemmatization errors if lemma attribute doesn't exists

    Code
      prep(rec)
    Error <recipes_error_step>
      
      Caused by error in `bake()`:
      ! `text` doesn't have a lemma attribute. Make sure the tokenization step includes lemmatization.

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
      Lemmatization for all_predictors()

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
      Lemmatization for text [trained]

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
