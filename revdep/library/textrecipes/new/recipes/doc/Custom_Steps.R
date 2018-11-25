## ----ex_setup, include=FALSE---------------------------------------------
knitr::opts_chunk$set(
  message = FALSE,
  digits = 3,
  collapse = TRUE,
  comment = "#>"
  )
options(digits = 3)

## ----step_list-----------------------------------------------------------
library(recipes)
ls("package:recipes", pattern = "^step_")

## ----initial-------------------------------------------------------------
data(biomass)
str(biomass)

biomass_tr <- biomass[biomass$dataset == "Training",]
biomass_te <- biomass[biomass$dataset == "Testing",]

## ----carbon_dist, fig.width=6, fig.height=4.25,  out.width = '100%'------
library(ggplot2)
theme_set(theme_bw())
ggplot(biomass_tr, aes(x = carbon)) + 
  geom_histogram(binwidth = 5, col = "blue", fill = "blue", alpha = .5) + 
  geom_vline(xintercept = biomass_te$carbon[1], lty = 2)

## ----initial_def---------------------------------------------------------
step_percentile <- function(
  recipe, ..., 
  role = NA, 
  trained = FALSE, 
  ref_dist = NULL,
  approx = FALSE, 
  options = list(probs = (0:100)/100, names = TRUE),
  skip = FALSE,
  id = rand_id("percentile")
  ) {

  ## The variable selectors are not immediately evaluated by using
  ##  the `quos` function in `rlang`. `ellipse_check` captures the
  ##  values and also checks to make sure that they are not empty.  
  terms <- ellipse_check(...) 

  add_step(
    recipe, 
    step_percentile_new(
      terms = terms, 
      trained = trained,
      role = role, 
      ref_dist = ref_dist,
      approx = approx,
      options = options,
      skip = skip,
      id = id
    )
  )
}

## ----initialize----------------------------------------------------------
step_percentile_new <- 
  function(terms, role, trained, ref_dist, approx, options, skip, id) {
    step(
      subclass = "percentile", 
      terms = terms,
      role = role,
      trained = trained,
      ref_dist = ref_dist,
      approx = approx,
      options = options,
      skip = skip,
      id = id
    )
  }

## ----prep_1, eval = FALSE------------------------------------------------
#  prep.step_percentile <- function(x, training, info = NULL, ...) {
#    col_names <- terms_select(terms = x$terms, info = info)
#  }

## ----prep_2--------------------------------------------------------------
get_pctl <- function(x, args) {
  args$x <- x
  do.call("quantile", args)
}

prep.step_percentile <- function(x, training, info = NULL, ...) {
  col_names <- terms_select(terms = x$terms, info = info) 
  ## You can add error trapping for non-numeric data here and so on. See the
  ## `check_type` function to do this for basic types. 
  
  ## We'll use the names later so
  if (x$options$names == FALSE)
    stop("`names` should be set to TRUE", call. = FALSE)
  
  if (!x$approx) {
    ref_dist <- training[, col_names]
  } else {
    ref_dist <- purrr::map(training[, col_names],  get_pctl, args = x$options)
  }

  ## Use the constructor function to return the updated object. 
  ## Note that `trained` is set to TRUE
  
  step_percentile_new(
    terms = x$terms, 
    trained = TRUE,
    role = x$role, 
    ref_dist = ref_dist,
    approx = x$approx,
    options = x$options,
    skip = x$skip,
    id = x$id
  )
}

## ----bake----------------------------------------------------------------
## Two helper functions
pctl_by_mean <- function(x, ref) mean(ref <= x)

pctl_by_approx <- function(x, ref) {
  ## go from 1 column tibble to vector
  x <- getElement(x, names(x))
  ## get the percentiles values from the names (e.g. "10%")
  p_grid <- as.numeric(gsub("%$", "", names(ref))) 
  approx(x = ref, y = p_grid, xout = x)$y/100
}

bake.step_percentile <- function(object, new_data, ...) {
  require(tibble)
  ## For illustration (and not speed), we will loop through the affected variables
  ## and do the computations
  vars <- names(object$ref_dist)
  
  for (i in vars) {
    if (!object$approx) {
      ## We can use `apply` since tibbles do not drop dimensions:
      new_data[, i] <- apply(new_data[, i], 1, pctl_by_mean, 
                            ref = object$ref_dist[, i])
    } else 
      new_data[, i] <- pctl_by_approx(new_data[, i], object$ref_dist[[i]])
  }
  ## Always convert to tibbles on the way out
  as_tibble(new_data)
}

## ----example-------------------------------------------------------------
library(purrr)
rec_obj <- 
  recipe(HHV ~ ., data = biomass_tr[, -(1:2)]) %>%
  step_percentile(all_predictors(), approx = TRUE) %>%
  prep(training = biomass_tr)

percentiles <- bake(rec_obj, biomass_te)
percentiles

## ----cdf_plot, echo = FALSE, fig.width=6, fig.height=4.25,  out.width = '100%'----
grid_pct <- rec_obj$steps[[1]]$options$probs
plot_data <- data.frame(
  carbon = c(
    quantile(biomass_tr$carbon, probs = grid_pct), 
    biomass_te$carbon
  ),
  percentile = c(grid_pct, percentiles$carbon),
  dataset = rep(
    c("Training", "Testing"), 
    c(length(grid_pct), nrow(percentiles))
  )
)

ggplot(plot_data, 
       aes(x = carbon, y = percentile, col = dataset)) + 
  geom_point(alpha = .4, cex = 2) + 
  theme(legend.position = "top")

