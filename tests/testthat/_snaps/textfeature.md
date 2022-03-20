# custom extraction functions work works

    Code
      rec %>% step_textfeature(text, extract_functions = list(as.character)) %>% prep()
    Error <vctrs_error_subscript_oob>
      Can't subset columns that don't exist.
      x Column `text` doesn't exist.

---

    Code
      rec %>% step_textfeature(text, extract_functions = list(function(x) 1)) %>%
        prep()
    Error <vctrs_error_subscript_oob>
      Can't subset columns that don't exist.
      x Column `text` doesn't exist.

# printing

    Code
      print(rec)
    Output
      Recipe
      
      Inputs:
      
            role #variables
       predictor          1
      
      Operations:
      
      Text feature extraction for text

# can prep recipes with no keep_original_cols

    Code
      koc_trained <- prep(koc_rec, training = test_data, verbose = FALSE)
    Warning <rlang_warning>
      'keep_original_cols' was added to `step_textfeature()` after this recipe was created.
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

