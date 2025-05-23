% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pos_filter.R
\name{step_pos_filter}
\alias{step_pos_filter}
\alias{tidy.step_pos_filter}
\title{Part of Speech Filtering of Token Variables}
\usage{
step_pos_filter(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  columns = NULL,
  keep_tags = "NOUN",
  skip = FALSE,
  id = rand_id("pos_filter")
)
}
\arguments{
\item{recipe}{A \link[recipes:recipe]{recipes::recipe} object. The step will be added to the
sequence of operations for this recipe.}

\item{...}{One or more selector functions to choose which
variables are affected by the step. See \code{\link[recipes:selections]{recipes::selections()}}
for more details.}

\item{role}{Not used by this step since no new variables are
created.}

\item{trained}{A logical to indicate if the quantities for
preprocessing have been estimated.}

\item{columns}{A character string of variable names that will
be populated (eventually) by the \code{terms} argument. This is \code{NULL}
until the step is trained by \code{\link[recipes:prep]{recipes::prep.recipe()}}.}

\item{keep_tags}{Character variable of part of speech tags to keep. See
details for complete list of tags. Defaults to "NOUN".}

\item{skip}{A logical. Should the step be skipped when the
recipe is baked by \code{\link[recipes:bake]{recipes::bake.recipe()}}? While all operations are baked
when \code{\link[recipes:prep]{recipes::prep.recipe()}} is run, some operations may not be able to be
conducted on new data (e.g. processing the outcome variable(s)).
Care should be taken when using \code{skip = FALSE}.}

\item{id}{A character string that is unique to this step to identify it.}
}
\value{
An updated version of \code{recipe} with the new step added
to the sequence of existing steps (if any).
}
\description{
\code{step_pos_filter()} creates a \emph{specification} of a recipe step that will
filter a \code{\link[=tokenlist]{token}} variable based on part of speech tags.
}
\details{
Possible part of speech tags for \code{spacyr} engine are: "ADJ", "ADP", "ADV",
"AUX", "CONJ", "CCONJ", "DET", "INTJ", "NOUN", "NUM", "PART", "PRON",
"PROPN", "PUNCT", "SCONJ", "SYM", "VERB", "X" and "SPACE". For more
information look here
\url{https://github.com/explosion/spaCy/blob/master/spacy/glossary.py}.
}
\section{Tidying}{
When you \code{\link[recipes:tidy.recipe]{tidy()}} this step, a tibble is returned with
columns \code{terms} and \code{id}:

\describe{
\item{terms}{character, the selectors or variables selected}
\item{id}{character, id of this step}
}
}

\section{Case weights}{


The underlying operation does not allow for case weights.
}

\examples{
\dontrun{
library(recipes)

short_data <- data.frame(text = c(
  "This is a short tale,",
  "With many cats and ladies."
))

rec_spec <- recipe(~text, data = short_data) |>
  step_tokenize(text, engine = "spacyr") |>
  step_pos_filter(text, keep_tags = "NOUN") |>
  step_tf(text)

rec_prepped <- prep(rec_spec)

bake(rec_prepped, new_data = NULL)
}

}
\seealso{
\code{\link[=step_tokenize]{step_tokenize()}} to turn characters into \code{\link[=tokenlist]{tokens}}

Other Steps for Token Modification: 
\code{\link{step_lemma}()},
\code{\link{step_ngram}()},
\code{\link{step_stem}()},
\code{\link{step_stopwords}()},
\code{\link{step_tokenfilter}()},
\code{\link{step_tokenmerge}()}
}
\concept{Steps for Token Modification}
