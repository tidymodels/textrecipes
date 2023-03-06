# tokenizer works

    Code
      vctrs::field(out, "lemma")
    Error <simpleError>
      Invalid index: field name 'lemma' not found

---

    Code
      vctrs::field(out, "pos")
    Error <simpleError>
      Invalid index: field name 'pos' not found

# Errors if vocabulary size is set to low.

    Code
      recipe(~text, data = tibble(text = "hello")) %>% step_tokenize(text, engine = "tokenizers.bpe",
        training_options = list(vocab_size = 2)) %>% prep()
    Error <recipes_error_step>
      Error in `step_tokenize()`:
      Caused by error in `prep()`:
      ! `vocabulary_size` of 2 is too small for column `text` which has a unique character count of 4

