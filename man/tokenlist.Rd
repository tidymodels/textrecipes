% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tokenlist.R
\name{tokenlist}
\alias{tokenlist}
\title{Create Token Object}
\usage{
tokenlist(tokens = list(), lemma = NULL, pos = NULL)
}
\arguments{
\item{tokens}{List of character vectors}

\item{lemma}{List of character vectors, must be same size and shape as \code{x}.}

\item{pos}{List of character vectors, must be same size and shape as \code{x}.}
}
\value{
a \link{tokenlist} object.
}
\description{
A \link{tokenlist} object is a thin wrapper around a list of character vectors,
with a few attributes.
}
\examples{
\dontshow{if (rlang::is_installed("modeldata")) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
abc <- list(letters, LETTERS)
tokenlist(abc)

unclass(tokenlist(abc))

tibble(text = tokenlist(abc))

library(tokenizers)
library(modeldata)
data(tate_text)
tokens <- tokenize_words(as.character(tate_text$medium))

tokenlist(tokens)
\dontshow{\}) # examplesIf}
}
