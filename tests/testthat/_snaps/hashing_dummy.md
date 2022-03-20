# printing

    Code
      print(rec)
    Output
      Recipe
      
      Inputs:
      
            role #variables
       predictor          2
      
      Operations:
      
      Feature hashing with sponsor_code

# can prep recipes with no keep_original_cols

    Code
      koc_trained <- prep(koc_rec, training = test_data, verbose = FALSE)
    Warning <rlang_warning>
      'keep_original_cols' was added to `step_dummy_hash()` after this recipe was created.
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
      
      Feature hashing with <none>

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
      
      Feature hashing with <none> [trained]

