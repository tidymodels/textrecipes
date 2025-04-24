# Errors if vocabulary size is set to low.

    Code
      prep(step_tokenize(recipe(~text, data = tibble(text = "hello")), text, engine = "tokenizers.bpe",
      training_options = list(vocab_size = 2)))
    Condition
      Error in `step_tokenize()`:
      Caused by error in `prep()`:
      ! `vocabulary_size` of 2 is too small for column `text` which has a unique character count of 4

