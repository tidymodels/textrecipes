% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/texthash.R
\name{step_texthash}
\alias{step_texthash}
\alias{tidy.step_texthash}
\title{Feature Hashing of Tokens}
\usage{
step_texthash(
  recipe,
  ...,
  role = "predictor",
  trained = FALSE,
  columns = NULL,
  signed = TRUE,
  num_terms = 1024L,
  prefix = "texthash",
  sparse = "auto",
  keep_original_cols = FALSE,
  skip = FALSE,
  id = rand_id("texthash")
)
}
\arguments{
\item{recipe}{A \link[recipes:recipe]{recipes::recipe} object. The step will be added to the
sequence of operations for this recipe.}

\item{...}{One or more selector functions to choose which
variables are affected by the step. See \code{\link[recipes:selections]{recipes::selections()}}
for more details.}

\item{role}{For model terms created by this step, what analysis
role should they be assigned?. By default, the function assumes
that the new columns created by the original variables will be
used as predictors in a model.}

\item{trained}{A logical to indicate if the quantities for
preprocessing have been estimated.}

\item{columns}{A character string of variable names that will
be populated (eventually) by the \code{terms} argument. This is \code{NULL}
until the step is trained by \code{\link[recipes:prep]{recipes::prep.recipe()}}.}

\item{signed}{A logical, indicating whether to use a signed hash-function to
reduce collisions when hashing. Defaults to TRUE.}

\item{num_terms}{An integer, the number of variables to output. Defaults to
1024.}

\item{prefix}{A character string that will be the prefix to the
resulting new variables. See notes below.}

\item{sparse}{A single string. Should the columns produced be sparse vectors.
Can take the values \code{"yes"}, \code{"no"}, and \code{"auto"}. If \code{sparse = "auto"}
then workflows can determine the best option. Defaults to \code{"auto"}.}

\item{keep_original_cols}{A logical to keep the original variables in the
output. Defaults to \code{FALSE}.}

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
\code{step_texthash()} creates a \emph{specification} of a recipe step that will
convert a \code{\link[=tokenlist]{token}} variable into multiple numeric variables
using the hashing trick.
}
\details{
Feature hashing, or the hashing trick, is a transformation of a text variable
into a new set of numerical variables. This is done by applying a hashing
function over the tokens and using the hash values as feature indices. This
allows for a low memory representation of the text. This implementation is
done using the MurmurHash3 method.

The argument \code{num_terms} controls the number of indices that the hashing
function will map to. This is the tuning parameter for this transformation.
Since the hashing function can map two different tokens to the same index,
will a higher value of \code{num_terms} result in a lower chance of collision.

The new components will have names that begin with \code{prefix}, then
the name of the variable, followed by the tokens all separated by
\code{-}. The variable names are padded with zeros. For example if
\code{prefix = "hash"}, and if \code{num_terms < 10}, their names will be
\code{hash1} - \code{hash9}. If \code{num_terms = 101}, their names will be
\code{hash001} - \code{hash101}.
}
\section{Tidying}{
When you \code{\link[recipes:tidy.recipe]{tidy()}} this step, a tibble is returned with
columns \code{terms}, value and \code{id}:

\describe{
\item{terms}{character, the selectors or variables selected}
\item{value}{logical, is it signed?}
\item{length}{integer, number of terms}
\item{id}{character, id of this step}
}
}

\section{Tuning Parameters}{
This step has 2 tuning parameters:
\itemize{
\item \code{signed}: Signed Hash Value (type: logical, default: TRUE)
\item \code{num_terms}: # Hash Features (type: integer, default: 1024)
}
}

\section{Sparse data}{


This step produces sparse columns if \code{sparse = "yes"} is being set. The
default value \code{"auto"} won't trigger production fo sparse columns if a recipe
is \code{\link[recipes:prep]{recipes::prep()}}ed, but allows for a workflow to toggle to \code{"yes"} or
\code{"no"} depending on whether the model supports \link[recipes:sparse_data]{recipes::sparse_data} and if
the model is is expected to run faster with the data.

The mechanism for determining how much sparsity is produced isn't perfect,
and there will be times when you want to manually overwrite by setting
\code{sparse = "yes"} or \code{sparse = "no"}.
}

\section{Case weights}{


The underlying operation does not allow for case weights.
}

\examples{
\dontshow{if (all(c("modeldata", "text2vec", "data.table") \%in\% rownames(installed.packages()))) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
\dontshow{library(data.table)}
\dontshow{data.table::setDTthreads(2)}
\dontshow{Sys.setenv("OMP_THREAD_LIMIT" = 2)}
library(recipes)
library(modeldata)
data(tate_text)

tate_rec <- recipe(~., data = tate_text) |>
  step_tokenize(medium) |>
  step_tokenfilter(medium, max_tokens = 10) |>
  step_texthash(medium)

tate_obj <- tate_rec |>
  prep()

bake(tate_obj, tate_text)

tidy(tate_rec, number = 3)
tidy(tate_obj, number = 3)
\dontshow{\}) # examplesIf}
}
\references{
Kilian Weinberger; Anirban Dasgupta; John Langford; Alex Smola;
Josh Attenberg (2009).
}
\seealso{
\code{\link[=step_tokenize]{step_tokenize()}} to turn characters into \code{\link[=tokenlist]{tokens}}
\code{\link[=step_text_normalization]{step_text_normalization()}} to perform text normalization.

Other Steps for Numeric Variables From Tokens: 
\code{\link{step_lda}()},
\code{\link{step_tf}()},
\code{\link{step_tfidf}()},
\code{\link{step_word_embeddings}()}
}
\concept{Steps for Numeric Variables From Tokens}
