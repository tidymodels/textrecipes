#' @importFrom recipes add_step step terms_select sel2char ellipse_check names0
#' @importFrom recipes printer check_type rand_id bake prep names0 is_trained
#' 
#' @importFrom tibble as_tibble tibble
#' 
#' @importFrom purrr map map_dfc map_chr map_lgl map_dfr pmap keep
#' 
#' @importFrom rlang %||% na_chr na_int expr na_lgl
#' 
#' @importFrom dplyr bind_cols inner_join mutate_all select rename_all 
#' @importFrom dplyr summarize_all
#' 
#' @importFrom generics tidy
#' 
#' @importFrom stopwords stopwords
#' 
#' @importFrom tidyr unnest
#' 
#' @importFrom Matrix sparseMatrix
#' 
#' @importFrom vctrs vec_assert new_vctr vec_cast vec_ptype_abbr
#' @importFrom vctrs obj_print_footer vec_restore
NULL

# nocov start
.onLoad <- function(libname, pkgname) {
  # This package has specific methods for the `tunable` generic. That generic
  # is defined in the `tune` package. As of R 4.0, we need to register them.
  textrecipes_exports <- getNamespaceExports(ns = "textrecipes")
  tunable_steps <- grep("tunable.step", textrecipes_exports, fixed = TRUE, 
                        value = TRUE)
  for (i in tunable_steps) {
    s3_register("dplyr::tune", i)
  }
}

s3_register <- function(generic, class, method = NULL) {
  stopifnot(is.character(generic), length(generic) == 1)
  stopifnot(is.character(class), length(class) == 1)
  
  pieces <- strsplit(generic, "::")[[1]]
  stopifnot(length(pieces) == 2)
  package <- pieces[[1]]
  generic <- pieces[[2]]
  
  caller <- parent.frame()
  
  get_method_env <- function() {
    top <- topenv(caller)
    if (isNamespace(top)) {
      asNamespace(environmentName(top))
    } else {
      caller
    }
  }
  get_method <- function(method, env) {
    if (is.null(method)) {
      get(paste0(generic, ".", class), envir = get_method_env())
    } else {
      method
    }
  }
  
  method_fn <- get_method(method)
  stopifnot(is.function(method_fn))
  
  # Always register hook in case package is later unloaded & reloaded
  setHook(
    packageEvent(package, "onLoad"),
    function(...) {
      ns <- asNamespace(package)
      
      # Refresh the method, it might have been updated by `devtools::load_all()`
      method_fn <- get_method(method)
      
      registerS3method(generic, class, method_fn, envir = ns)
    }
  )
  
  # Avoid registration failures during loading (pkgload or regular)
  if (!isNamespaceLoaded(package)) {
    return(invisible())
  }
  
  envir <- asNamespace(package)
  
  # Only register if generic can be accessed
  if (exists(generic, envir)) {
    registerS3method(generic, class, method_fn, envir = envir)
  }
  
  invisible()
}

# nocov end