# tokenfilter removes words correctly using min_times and max_times

    Code
      obj <- rec %>% prep()
    Warning <rlang_warning>
      max_tokens was set to '100', but only 3 was available and selected.

# removes words correctly with min_times, max_times and procentage

    Code
      obj <- rec %>% prep()
    Warning <rlang_warning>
      max_tokens was set to '100', but only 12 was available and selected.

# tokenfilter throws warning when max_tokens > words

    Code
      rec %>% prep()
    Warning <rlang_warning>
      max_tokens was set to '10000', but only 17 was available and selected.
    Output
      Recipe
      
      Inputs:
      
            role #variables
       predictor          1
      
      Training data contained 4 data points and no missing data.
      
      Operations:
      
      Tokenization for text [trained]
      Text filtering for text [trained]

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
      Text filtering for text

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
      
      Text filtering for <none>

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
      
      Text filtering for <none> [trained]

