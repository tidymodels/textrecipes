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
      Stop word removal for text

---

    Code
      prep(rec, verbose = TRUE)
    Output
      oper 1 step tokenize [training] 
      oper 2 step stopwords [training] 
      The retained training set is ~ 0 Mb  in memory.
      
      Recipe
      
      Inputs:
      
            role #variables
       predictor          1
      
      Training data contained 4 data points and no missing data.
      
      Operations:
      
      Tokenization for text [trained]
      Stop word removal for text [trained]

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
      
      Stop word removal for <none>

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
      
      Stop word removal for <none> [trained]

