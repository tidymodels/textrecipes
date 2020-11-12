skip_if_no_python_or_no_spacy <- function() {
  if (spacyr::find_spacy_env()) {
    return(NULL)
  }
  spacy_path <- spacyr::find_spacy(ask = FALSE)
  if (is.null(spacy_path)) {
    testthat::skip("Skip the test as spaCy is not found")
  } else if (is.na(spacy_path)) {
    testthat::skip("Skip the test as python is not found")
  }
}
