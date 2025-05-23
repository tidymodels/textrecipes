% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dummy_hash.R
\name{step_dummy_hash}
\alias{step_dummy_hash}
\alias{tidy.step_dummy_hash}
\title{Indicator Variables via Feature Hashing}
\usage{
step_dummy_hash(
  recipe,
  ...,
  role = "predictor",
  trained = FALSE,
  columns = NULL,
  signed = TRUE,
  num_terms = 32L,
  collapse = FALSE,
  prefix = "dummyhash",
  sparse = "auto",
  keep_original_cols = FALSE,
  skip = FALSE,
  id = rand_id("dummy_hash")
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

\item{signed}{A logical, indicating whether to use a signed hash-function
(generating values of -1, 0, or 1), to reduce collisions when hashing.
Defaults to TRUE.}

\item{num_terms}{An integer, the number of variables to output. Defaults to
32.}

\item{collapse}{A logical; should all of the selected columns be collapsed
into a single column to create a single set of hashed features?}

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
\code{step_dummy_hash()} creates a \emph{specification} of a recipe step that will
convert factors or character columns into a series of binary (or signed
binary) indicator columns.
}
\details{
Feature hashing, or the hashing trick, is a transformation of a text variable
into a new set of numerical variables. This is done by applying a hashing
function over the values of the factor levels and using the hash values as
feature indices. This allows for a low memory representation of the data and
can be very helpful when a qualitative predictor has many levels or is
expected to have new levels during prediction. This implementation is done
using the MurmurHash3 method.

The argument \code{num_terms} controls the number of indices that the hashing
function will map to. This is the tuning parameter for this transformation.
Since the hashing function can map two different tokens to the same index,
a higher value of \code{num_terms} will result in a lower chance of collision.

The new components will have names that begin with \code{prefix}, then
the name of the variable, followed by the tokens all separated by
\code{-}. The variable names are padded with zeros. For example if
\code{prefix = "hash"}, and if \code{num_terms < 10}, their names will be
\code{hash1} - \code{hash9}. If \code{num_terms = 101}, their names will be
\code{hash001} - \code{hash101}.
}
\section{Tidying}{
When you \code{\link[recipes:tidy.recipe]{tidy()}} this step, a tibble is returned with
columns \code{terms}, \code{value}, \code{num_terms}, \code{collapse}, and \code{id}:

\describe{
\item{terms}{character, the selectors or variables selected}
\item{value}{logical, whether a signed hashing was performed}
\item{num_terms}{integer, number of terms}
\item{collapse}{logical, were the columns collapsed}
\item{id}{character, id of this step}
}
}

\section{Tuning Parameters}{
This step has 2 tuning parameters:
\itemize{
\item \code{signed}: Signed Hash Value (type: logical, default: TRUE)
\item \code{num_terms}: # Hash Features (type: integer, default: 32)
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
\dontshow{Sys.setenv("OMP_NUM_THREADS" = 1)}
\dontshow{Sys.setenv("OMP_THREAD_LIMIT" = 1)}
\dontshow{Sys.setenv("rsparse_omp_threads" = 1L)}
\dontshow{options(rsparse_omp_threads = 1L)}
\dontshow{library(text2vec)}
\dontshow{Sys.setenv("OMP_NUM_THREADS" = 1)}
\dontshow{Sys.setenv("OMP_THREAD_LIMIT" = 1)}
\dontshow{Sys.setenv("rsparse_omp_threads" = 1L)}
\dontshow{options(rsparse_omp_threads = 1L)}
\dontshow{options("text2vec.mc.cores" = 1)}

library(recipes)
library(modeldata)
data(grants)

grants_rec <- recipe(~sponsor_code, data = grants_other) |>
  step_dummy_hash(sponsor_code)

grants_obj <- grants_rec |>
  prep()

bake(grants_obj, grants_test)

tidy(grants_rec, number = 1)
tidy(grants_obj, number = 1)
\dontshow{\}) # examplesIf}
}
\references{
Kilian Weinberger; Anirban Dasgupta; John Langford; Alex Smola;
Josh Attenberg (2009).

Kuhn and Johnson (2019), Chapter 7,
\url{https://bookdown.org/max/FES/encoding-predictors-with-many-categories.html}
}
\seealso{
\code{\link[recipes:step_dummy]{recipes::step_dummy()}}

Other Steps for Numeric Variables From Characters: 
\code{\link{step_sequence_onehot}()},
\code{\link{step_textfeature}()}
}
\concept{Steps for Numeric Variables From Characters}
