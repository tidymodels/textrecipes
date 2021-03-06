% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sequence_onehot.R
\name{step_sequence_onehot}
\alias{step_sequence_onehot}
\alias{tidy.step_sequence_onehot}
\title{Generate the basic set of text features}
\source{
\url{https://papers.nips.cc/paper/5782-character-level-convolutional-networks-for-text-classification.pdf}
}
\usage{
step_sequence_onehot(
  recipe,
  ...,
  role = "predictor",
  trained = FALSE,
  columns = NULL,
  sequence_length = 100,
  padding = "pre",
  truncating = "pre",
  vocabulary = NULL,
  prefix = "seq1hot",
  skip = FALSE,
  id = rand_id("sequence_onehot")
)

\method{tidy}{step_sequence_onehot}(x, ...)
}
\arguments{
\item{recipe}{A recipe object. The step will be added to the
sequence of operations for this recipe.}

\item{...}{One or more selector functions to choose variables.
For \code{step_sequence_onehot}, this indicates the variables to be encoded
into a \link{tokenlist}. See \code{\link[recipes:selections]{recipes::selections()}} for more
details. For the \code{tidy} method, these are not currently used.}

\item{role}{For model terms created by this step, what analysis
role should they be assigned?. By default, the function assumes
that the new columns created by the original variables will be
used as predictors in a model.}

\item{trained}{A logical to indicate if the recipe has been
baked.}

\item{columns}{A list of tibble results that define the
encoding. This is \code{NULL} until the step is trained by
\code{\link[recipes:prep]{recipes::prep.recipe()}}.}

\item{sequence_length}{A numeric, number of characters to keep before
discarding. Defaults to 100.}

\item{padding}{'pre' or 'post', pad either before or after each sequence.
defaults to 'pre'.}

\item{truncating}{'pre' or 'post', remove values from sequences larger than
sequence_length either in the beginning or in the end of the sequence.
Defaults too 'pre'.}

\item{vocabulary}{A character vector, characters to be mapped to integers.
Characters not in the vocabulary will be encoded as 0. Defaults to
\code{letters}.}

\item{prefix}{A prefix for generated column names, default to "seq1hot".}

\item{skip}{A logical. Should the step be skipped when the
recipe is baked by \code{\link[recipes:bake]{recipes::bake.recipe()}}? While all
operations are baked when \code{\link[recipes:prep]{recipes::prep.recipe()}} is run, some
operations may not be able to be conducted on new data (e.g.
processing the outcome variable(s)). Care should be taken when
using \code{skip = TRUE} as it may affect the computations for
subsequent operations.}

\item{id}{A character string that is unique to this step to identify it}

\item{x}{A \code{step_sequence_onehot} object.}
}
\value{
An updated version of \code{recipe} with the new step added
to the sequence of existing steps (if any).
}
\description{
\code{step_sequence_onehot} creates a \emph{specification} of a recipe step that
will take a string and do one hot encoding for each character by position.
}
\details{
The string will be capped by the sequence_length argument, strings shorter then
sequence_length will be padded with empty characters. The encoding will assign
a integer to each character in the vocabulary, and will encode accordingly.
Characters not in the vocabulary will be encoded as 0.
}
\examples{
library(recipes)
library(modeldata)
data(okc_text)

okc_rec <- recipe(~essay0, data = okc_text) \%>\%
  step_tokenize(essay0) \%>\%
  step_tokenfilter(essay0) \%>\%
  step_sequence_onehot(essay0)

okc_obj <- okc_rec \%>\%
  prep()

bake(okc_obj, new_data = NULL)

tidy(okc_rec, number = 1)
tidy(okc_obj, number = 1)
}
\seealso{
Other character to numeric steps: 
\code{\link{step_lda}()},
\code{\link{step_textfeature}()}
}
\concept{character to numeric steps}
