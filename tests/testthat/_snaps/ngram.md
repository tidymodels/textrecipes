# ngram works with varrying number of `n`

    Code
      rcpp_ngram(test_data, n = 0L, n_min = 0L, delim = "_")
    Error <Rcpp::exception>
      'n' must be a positive integer.

---

    Code
      rcpp_ngram(test_data, n = -1L, n_min = -1L, delim = "_")
    Error <Rcpp::exception>
      'n' must be a positive integer.

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
      ngramming for text

---

    Code
      prep(rec)
    Output
      Recipe
      
      Inputs:
      
            role #variables
       predictor          1
      
      Training data contained 2 data points and no missing data.
      
      Operations:
      
      Tokenization for text [trained]
      ngramming for text [trained]

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
      
      ngramming for <none>

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
      
      ngramming for <none> [trained]

