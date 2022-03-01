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
      Word embeddings aggregated from text

---

    Code
      prep(rec, verbose = TRUE)
    Output
      oper 1 step tokenize [training] 
      oper 2 step word embeddings [training] 
      The retained training set is ~ 0 Mb  in memory.
      
      Recipe
      
      Inputs:
      
            role #variables
       predictor          1
      
      Training data contained 4 data points and no missing data.
      
      Operations:
      
      Tokenization for text [trained]
      Word embeddings aggregated from text [trained]

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
      
      Word embeddings aggregated from <none>

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
      
      Word embeddings aggregated from <none> [trained]

