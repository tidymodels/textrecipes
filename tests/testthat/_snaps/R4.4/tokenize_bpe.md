# Errors if vocabulary size is set to low.

    Code
      recipe(~text1, data = test_data) %>% step_tokenize_bpe(text1, vocabulary_size = 10) %>%
        prep()
    Condition
      Error in `step_tokenize_bpe()`:
      Caused by error in `prep()`:
      ! `vocabulary_size` of 10 is too small for column `text1` which has a unique character count of 23

