# it complains when the selected column isn't a tokenlist

    Code
      prep(rec)
    Error <rlang_error>
      All columns selected for this step should be tokenlists

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
      
      Merging tokens for <none>

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
      
      Merging tokens for <none> [trained]

