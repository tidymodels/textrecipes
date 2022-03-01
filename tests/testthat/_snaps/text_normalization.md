# printing

    Code
      print(rec)
    Output
      Recipe
      
      Inputs:
      
            role #variables
       predictor          1
      
      Operations:
      
      Text Normalization for text

---

    Code
      prep(rec, training = ex_dat, verbose = TRUE)
    Output
      oper 1 step text normalization [training] 
      The retained training set is ~ 0 Mb  in memory.
      
      Recipe
      
      Inputs:
      
            role #variables
       predictor          1
      
      Training data contained 2 data points and no missing data.
      
      Operations:
      
      Text Normalization for text [trained]

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
      
      Text Normalization for <none>

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
      
      Text Normalization for <none> [trained]

