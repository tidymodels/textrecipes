# bad args

    Code
      step_token_replace(rec, text, pattern = 1, replacement = "a")
    Condition
      Error in `step_token_replace()`:
      ! `pattern` must be a character vector, not the number 1.

---

    Code
      step_token_replace(rec, text, pattern = c("a", "b"), replacement = c("a", "b",
        "c"))
    Condition
      Error in `step_token_replace()`:
      ! `replacement` must be length 1 or the same length as `pattern` (2), not length 3.

---

    Code
      step_token_replace(rec, text, pattern = "a", replacement = "b", fixed = "yes")
    Condition
      Error in `step_token_replace()`:
      ! `fixed` must be `TRUE` or `FALSE`, not the string "yes".

---

    Code
      step_token_replace(rec, text)
    Condition
      Error in `step_token_replace()`:
      ! `pattern` must be a character vector, not absent.

---

    Code
      step_token_replace(rec, text, pattern = NA_character_, replacement = "b")
    Condition
      Error in `step_token_replace()`:
      ! `pattern` can't contain NA values.

---

    Code
      step_token_replace(rec, text, pattern = "a", replacement = NA_character_)
    Condition
      Error in `step_token_replace()`:
      ! `replacement` can't contain NA values.

---

    Code
      step_token_replace(rec, text, pattern = "[", replacement = "b", fixed = FALSE)
    Condition
      Error in `step_token_replace()`:
      ! `pattern` contains an invalid regular expression: "[".

---

    Code
      step_token_replace(rec, text, pattern = character(), replacement = character())
    Condition
      Error in `step_token_replace()`:
      ! `pattern` must have at least one element.

---

    Code
      step_token_replace(rec, text, pattern = "", replacement = "b", fixed = FALSE)
    Condition
      Error in `step_token_replace()`:
      ! `pattern` must not contain empty strings.

---

    Code
      prep(step_token_replace(rec, text, pattern = "a", replacement = "b"))
    Condition
      Error in `step_token_replace()`:
      Caused by error in `prep()`:
      x All columns selected for the step should be tokenlist.
      * 1 factor variable found: `text`

# bake method errors when needed non-standard role columns are missing

    Code
      bake(trained, new_data = tokenized_test_data[, -1])
    Condition
      Error in `step_token_replace()`:
      ! The following required column is missing from `new_data`: text.

# empty printing

    Code
      rec
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:    1
      predictor: 10
      
      -- Operations 
      * Token replacement for: <none>

---

    Code
      rec
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:    1
      predictor: 10
      
      -- Training information 
      Training data contained 32 data points and no incomplete rows.
      
      -- Operations 
      * Token replacement for: <none> | Trained

# printing

    Code
      print(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 1
      
      -- Operations 
      * Tokenization for: text
      * Token replacement for: text

---

    Code
      prep(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 1
      
      -- Training information 
      Training data contained 4 data points and no incomplete rows.
      
      -- Operations 
      * Tokenization for: text | Trained
      * Token replacement for: text | Trained

