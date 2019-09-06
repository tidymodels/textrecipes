# textrecipes 0.0.2

* Custom stemming function can now be used in step_stem using the custom_stemmer argument.
* `step_textfeatures()` have been added, allows for multiple numerical features to be pulled from text.
* `step_sequence_onehot()` have been added, allows for one hot encoding of sequences of fixed width.
* `step_word2vec()` have been added, calculates word2vec dimensions.
* `step_tokenmerge()` have been added, combines multiple list columns into one list-columns.
* `step_texthash()` now correctly accepts `signed` argument.
* Documentation have been improved to showcase the importance of filtering tokens before applying `step_tf()` and `step_tfidf()`.

# textrecipes 0.0.1
 
First CRAN version
