% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tokenfilter.R
\name{step_tokenfilter}
\alias{step_tokenfilter}
\alias{tidy.step_tokenfilter}
\title{Filter Tokens Based on Term Frequency}
\usage{
step_tokenfilter(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  columns = NULL,
  max_times = Inf,
  min_times = 0,
  percentage = FALSE,
  max_tokens = 100,
  filter_fun = NULL,
  res = NULL,
  skip = FALSE,
  id = rand_id("tokenfilter")
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

\item{max_times}{An integer. Maximal number of times a word can appear before
getting removed.}

\item{min_times}{An integer. Minimum number of times a word can appear before
getting removed.}

\item{percentage}{A logical. Should max_times and min_times be interpreted as
a percentage instead of count.}

\item{max_tokens}{An integer. Will only keep the top max_tokens tokens after
filtering done by max_times and min_times. Defaults to 100.}

\item{filter_fun}{A function. This function should take a vector of
characters, and return a logical vector of the same length. This function
will be applied to each observation of the data set. Defaults to \code{NULL}.
All other arguments will be ignored if this argument is used.}

\item{res}{The words that will be keep will be stored here once this
preprocessing step has be trained by \code{\link[recipes:prep]{recipes::prep.recipe()}}.}

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
\code{step_tokenfilter()} creates a \emph{specification} of a recipe step that will
convert a \code{\link[=tokenlist]{token}} variable to be filtered based on frequency.
}
\details{
This step allows you to limit the tokens you are looking at by filtering on
their occurrence in the corpus. You are able to exclude tokens if they appear
too many times or too few times in the data. It can be specified as counts
using \code{max_times} and \code{min_times} or as percentages by setting \code{percentage}
as \code{TRUE}. In addition one can filter to only use the top \code{max_tokens} used
tokens. If \code{max_tokens} is set to \code{Inf} then all the tokens will be used.
This will generally lead to very large data sets when then tokens are words
or trigrams. A good strategy is to start with a low token count and go up
according to how much RAM you want to use.

It is strongly advised to filter before using \link{step_tf} or \link{step_tfidf} to
limit the number of variables created.
}
\section{Tidying}{
When you \code{\link[recipes:tidy.recipe]{tidy()}} this step, a tibble is returned with
columns \code{terms}, \code{value}, and \code{id}:

\describe{
\item{terms}{character, the selectors or variables selected}
\item{value}{integer, number of unique tokens}
\item{id}{character, id of this step}
}
}

\section{Tuning Parameters}{
This step has 3 tuning parameters:
\itemize{
\item \code{max_times}: Maximum Token Frequency (type: integer, default: Inf)
\item \code{min_times}: Minimum Token Frequency (type: integer, default: 0)
\item \code{max_tokens}: # Retained Tokens (type: integer, default: 100)
}
}

\section{Case weights}{


The underlying operation does not allow for case weights.
}

\examples{
\dontshow{if (rlang::is_installed("modeldata")) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
library(recipes)
library(modeldata)
data(tate_text)

tate_rec <- recipe(~., data = tate_text) |>
  step_tokenize(medium) |>
  step_tokenfilter(medium)

tate_obj <- tate_rec |>
  prep()

bake(tate_obj, new_data = NULL, medium) |>
  slice(1:2)

bake(tate_obj, new_data = NULL) |>
  slice(2) |>
  pull(medium)

tidy(tate_rec, number = 2)
tidy(tate_obj, number = 2)
\dontshow{\}) # examplesIf}
}
\seealso{
\code{\link[=step_tokenize]{step_tokenize()}} to turn characters into \code{\link[=tokenlist]{tokens}}

Other Steps for Token Modification: 
\code{\link{step_lemma}()},
\code{\link{step_ngram}()},
\code{\link{step_pos_filter}()},
\code{\link{step_stem}()},
\code{\link{step_stopwords}()},
\code{\link{step_tokenmerge}()}
}
\concept{Steps for Token Modification}
