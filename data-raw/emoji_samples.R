## code to prepare `emoji_samples` dataset goes here
library(tibble)
emoji_samples <- 
  tibble(
    text = c("my ⏰ didn’t work.",
             "I saw an 🚑.",
             "tokenization is 🔥🔥🔥",
             "Love is in the air 🧑🏻‍❤️‍💋‍🧑🏼")
  )

usethis::use_data(emoji_samples, overwrite = TRUE)
