# show_tokens() errors clearly when `n` is above nrow(rec$template)

    Code
      show_tokens(rec, text, n = 100)
    Condition
      Error in `show_tokens()`:
      ! `n` must be a whole number between 0 and 2, not the number 100.

# show_tokens() errors clearly when `n` is below the min bound

    Code
      show_tokens(rec, text, n = -1)
    Condition
      Error in `show_tokens()`:
      ! `n` must be a whole number between 0 and 2, not the number -1.

# show_tokens() errors clearly when `n` isn't a whole number

    Code
      show_tokens(rec, text, n = 2.5)
    Condition
      Error in `show_tokens()`:
      ! `n` must be a whole number, not the number 2.5.

