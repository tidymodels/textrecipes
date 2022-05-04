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
      prep(rec)
    Output
      Recipe
      
      Inputs:
      
            role #variables
       predictor          2
      
      Training data contained 100 data points and no missing data.
      
      Operations:
      
      Tokenization for medium [trained]
      Text feature extraction for medium [trained]

# can prep recipes with no keep_original_cols

    Code
      koc_trained <- prep(koc_rec, training = tate_text, verbose = FALSE)
    Warning <rlang_warning>
      'keep_original_cols' was added to `step_lda()` after this recipe was created.
      Regenerate your recipe to avoid this warning.

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

