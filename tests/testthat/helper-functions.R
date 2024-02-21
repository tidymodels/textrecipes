skip_if_no_python_or_no_spacy <- function() {
  if (reticulate::py_module_available("spacy")) {
    return(NULL)
  } else {
    testthat::skip("Skip the test as spaCy is not found")
  }
}
