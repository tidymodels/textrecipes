# Errors if vocabulary size is set to low.

    Code
      recipe(~text1, data = test_data) %>% step_tokenize_bpe(text1, vocabulary_size = 10) %>%
        prep()
    Condition
      Warning in `read.dcf()`:
      cannot open compressed file '/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/library/tokenizers.bpe/DESCRIPTION', probable reason 'No such file or directory'
    Message
      1 package (tokenizers.bpe) is needed for this step but is not installed.
      To install run: `install.packages("tokenizers.bpe")`
    Condition
      Error in `step_tokenize_bpe()`:
      Caused by error in `prep()`:
      ! `vocabulary_size` of 10 is too small for column `text1` which has a unique character count of 23

