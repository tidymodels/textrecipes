# Takes a data.frame (data) and replaces the columns with the names (names)
# and converts them from factor variable to character variables. Keeps 
# characters variables unchanged.
factor_to_text <- function(data, names) {
  for (i in seq_along(names)) {
    if(is.factor(data[, names[i], drop = TRUE]))
      data[, names[i]] <- as.character.factor(data[, names[i], drop = TRUE])
  }
  data
}

## This function takes the default arguments of `func` and
## replaces them with the matching ones in `options` and
## remove any in `removals`
sub_args <- function(func, options, removals = NULL) {
  args <- formals(func)
  for (i in seq_along(options))
    args[[names(options)[i]]] <- options[[i]]
  if (!is.null(removals))
    args[removals] <- NULL
  args
}
## Same as above but starts with a call object
mod_call_args <- function(cl, args, removals = NULL) {
  if (!is.null(removals))
    for (i in removals)
      cl[[i]] <- NULL
    arg_names <- names(args)
    for (i in arg_names)
      cl[[i]] <- args[[i]]
    cl
}

check_list <- function (dat) {

  all_good <- vapply(dat, is.list, logical(1))
  label <- "numeric"

  if (!all(all_good)) 
    stop("All columns selected for the step should be a list-column", 
         call. = FALSE)
  invisible(all_good)
}
