#' @importFrom generics tidy
#' @importFrom Matrix sparseMatrix
#' @importFrom purrr keep map map_chr map_dfc map_dfr map_lgl pmap
#' @importFrom recipes add_step bake check_type ellipse_check is_trained names0
#' @importFrom recipes prep printer rand_id sel2char step recipes_eval_select
#' @importFrom rlang .data %||% expr na_chr na_int na_lgl :=
#' @importFrom tibble as_tibble enframe tibble
#' @importFrom tidyr unnest
#' @importFrom vctrs new_vctr obj_print_footer vec_assert vec_cast
#' @importFrom vctrs vec_ptype_abbr vec_restore
NULL

utils::globalVariables("tokens")
