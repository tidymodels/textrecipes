# printing

    Code
      print(rec)
    Output
      Recipe
      
      Inputs:
      
            role #variables
       predictor          2
      
      Operations:
      
      Tokenization for medium
      Text feature extraction for medium

---

    Code
      prep(rec, verbose = TRUE)
    Output
      oper 1 step tokenize [training] 
      oper 2 step lda [training] 
      The retained training set is ~ 0.06 Mb  in memory.
      
      Recipe
      
      Inputs:
      
            role #variables
       predictor          2
      
      Training data contained 100 data points and no missing data.
      
      Operations:
      
      Tokenization for medium [trained]
      Text feature extraction for medium [trained]

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
      
      Text feature extraction for <none>

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
      
      Text feature extraction for <none> [trained]

