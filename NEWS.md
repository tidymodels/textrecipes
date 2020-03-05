# textrecipes 0.1.0

* `step_word2vec()` have been changed to `step_lda()` to reflect what is actually happening.
* `step_word_embeddings()` has been added. Allows for use of pre-trained word embeddings to convert token columns to vectors in a high-dimensional "meaning" space. (@jonthegeek, #20)
* text2vec have been changed from Imports to Suggests.
* textfeatures have been changed from Imports to Suggests.
* `step_tfidf()` calculations are slightly changed due to flaw in original implementation https://github.com/dselivanov/text2vec/issues/280.

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
