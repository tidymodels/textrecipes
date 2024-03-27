n_words <- function(x) {
  stringi::stri_count_words(x)
}

n_uq_words <- function(x) {
  x <- stringi::stri_extract_all_words(x)
  purrr::map_int(x, dplyr::n_distinct)
}

n_charS <- function(x) {
  x <- stringi::stri_replace_all_regex(x, "\\s", "")
  nchar(x)
}

n_uq_charS <- function(x) {
  x <- stringi::stri_replace_all_regex(x, "\\s", "")
  x <- stringi::stri_split_boundaries(
    x,
    opts_brkiter = stringi::stri_opts_brkiter(type = "character")
  )
  purrr::map_int(x, dplyr::n_distinct)
}

n_digits <- function(x) {
  stringi::stri_count_regex(x, "\\d")
}


n_hashtags <- function(x) {
  stringi::stri_count_regex(x, "#[[:alnum:]_]+")
}

n_uq_hashtags <- function(x) {
  x <- stringi::stri_extract_all_regex(x, "#[[:alnum:]_]+", omit_no_match = TRUE)
  purrr::map_int(x, dplyr::n_distinct)
}

n_mentions <- function(x) {
  stringi::stri_count_regex(x, "@\\S+")
}

n_uq_mentions <- function(x) {
  x <- stringi::stri_extract_all_regex(x, "@\\S+", omit_no_match = TRUE)
  purrr::map_int(x, dplyr::n_distinct)
}

n_commas <- function(x) {
  stringi::stri_count_fixed(x, ",")
}

n_periods <- function(x) {
  stringi::stri_count_fixed(x, ".")
}

n_exclaims <- function(x) {
  stringi::stri_count_fixed(x, "!")
}

n_extraspaces <- function(x) {
  stringi::stri_count_regex(x, "\\s{2}|\\t|\\n")
}

n_caps <- function(x) {
  stringi::stri_count_regex(x, "[[:upper:]]")
}

n_lowers <- function(x) {
  stringi::stri_count_regex(x, "[[:lower:]]")
}

n_urls <- function(x) {
  stringi::stri_count_regex(x, "https?")
}

n_uq_urls <- function(x) {
  x <- stringi::stri_extract_all_regex(x, "https?", omit_no_match = TRUE)
  purrr::map_int(x, dplyr::n_distinct)
}

n_nonasciis <- function(x) {
  x <- iconv(x, from = "UTF-8", to = "ASCII", sub = "[NONASCII]")
  stringi::stri_count_regex(x, "\\[NONASCII\\]")
}

n_puncts <- function(x) {
  x <- stringi::stri_replace_all_regex(x, "!|\\.|\\,", "")
  stringi::stri_count_regex(x, "[[:punct:]]")
}

first_person <- function(x) {
  fp <- c("i", "me", "myself", "my", "mine", "this")
  purrr::map_int(x, ~ sum(fp %in% .x, na.rm = TRUE))
}

first_personp <- function(x) {
  fp <- c("we", "us", "our", "ours", "these")
  purrr::map_int(x, ~ sum(fp %in% .x, na.rm = TRUE))
}

second_person <- function(x) {
  fp <- c("you", "yours", "your", "yourself")
  purrr::map_int(x, ~ sum(fp %in% .x, na.rm = TRUE))
}

second_personp <- function(x) {
  fp <- c("he", "she", "it", "its", "his", "hers")
  purrr::map_int(x, ~ sum(fp %in% .x, na.rm = TRUE))
}

third_person <- function(x) {
  fp <- c("they", "them", "theirs", "their", "they're",
          "their's", "those", "that")
  purrr::map_int(x, ~ sum(fp %in% .x, na.rm = TRUE))
}

to_be <- function(x) {
  fp <- c("am", "is", "are", "was", "were", "being",
          "been", "be", "were", "be")
  purrr::map_int(x, ~ sum(fp %in% .x, na.rm = TRUE))
}

prepositions <- function(x) {
  fp <- c("about", "below", "excepting", "off", "toward", "above", "beneath",
          "on", "under", "across", "from", "onto", "underneath", "after", "between",
          "in", "out", "until", "against", "beyond", "outside", "up", "along", "but",
          "inside", "over", "upon", "among", "by", "past", "around", "concerning",
          "regarding", "with", "at", "despite", "into", "since", "within", "down",
          "like", "through", "without", "before", "during", "near", "throughout",
          "behind", "except", "of", "to", "for")
  purrr::map_int(x, ~ sum(fp %in% .x, na.rm = TRUE))
}

#' List of all feature counting functions
#'
#' @details
#' In this function we refer to "first person", "first person plural" and
#' so on. This list describes what words are contained in each group.
#' \describe{
#' \item{first person}{I, me, myself, my, mine, this.}
#' \item{first person plural}{we, us, our, ours, these.}
#' \item{second person}{you, yours, your, yourself.}
#' \item{second person plural}{he, she, it, its, his, hers.}
#' \item{third person}{they, them, theirs, their, they're, their's, those, that.}
#' \item{to be}{am, is, are, was, were, being, been, be, were, be.}
#' \item{prepositions}{about, below,
#' excepting, off, toward, above, beneath, on, under, across, from, onto,
#' underneath, after, between, in, out, until, against, beyond, outside, up,
#' along, but, inside, over, upon, among, by, past, around, concerning,
#' regarding, with, at, despite, into, since, within, down, like, through,
#' without, before, during, near, throughout, behind, except, of, to, for.}
#' }
#'
#' @export
#' @format Named list of all ferature counting functions
#' \describe{
#' \item{\code{n_words}}{Number of words.}
#' \item{\code{n_uq_words}}{Number of unique words.}
#' \item{\code{n_charS}}{Number of characters. Not counting urls, hashtags, mentions or white spaces.}
#' \item{\code{n_uq_charS}}{Number of unique characters. Not counting urls, hashtags, mentions or white spaces.}
#' \item{\code{n_digits}}{Number of digits.}
#' \item{\code{n_hashtags}}{Number of hashtags, word preceded by a '#'.}
#' \item{\code{n_uq_hashtags}}{Number of unique hashtags, word preceded by a '#'.}
#' \item{\code{n_mentions}}{Number of mentions, word preceded by a '@@'.}
#' \item{\code{n_uq_mentions}}{Number of unique mentions, word preceded by a '@@'.}
#' \item{\code{n_commas}}{Number of commas.}
#' \item{\code{n_periods}}{Number of periods.}
#' \item{\code{n_exclaims}}{Number of exclamation points.}
#' \item{\code{n_extraspaces}}{Number of times more then 1 consecutive space have been used.}
#' \item{\code{n_caps}}{Number of upper case characters.}
#' \item{\code{n_lowers}}{Number of lower case characters.}
#' \item{\code{n_urls}}{Number of urls.}
#' \item{\code{n_uq_urls}}{Number of unique urls.}
#' \item{\code{n_nonasciis}}{Number of non ascii characters.}
#' \item{\code{n_puncts}}{Number of punctuations characters, not including exclamation points, periods and commas.}
#' \item{\code{first_person}}{Number of "first person" words.}
#' \item{\code{first_personp}}{Number of "first person plural" words.}
#' \item{\code{second_person}}{Number of "second person" words.}
#' \item{\code{second_personp}}{Number of "second person plural" words.}
#' \item{\code{third_person}}{Number of "third person" words.}
#' \item{\code{to_be}}{Number of "to be" words.}
#' \item{\code{prepositions}}{Number of preposition words.}
#' }
count_functions <- list(
  n_words = n_words,
  n_uq_words = n_uq_words,
  n_charS = n_charS,
  n_uq_charS = n_uq_charS,
  n_digits = n_digits,
  n_hashtags = n_hashtags,
  n_uq_hashtags = n_uq_hashtags,
  n_mentions = n_mentions,
  n_uq_mentions = n_uq_mentions,
  n_commas = n_commas,
  n_periods = n_periods,
  n_exclaims = n_exclaims,
  n_extraspaces = n_extraspaces,
  n_caps = n_caps,
  n_lowers = n_lowers,
  n_urls = n_urls,
  n_uq_urls = n_uq_urls,
  n_nonasciis = n_nonasciis,
  n_puncts = n_puncts,
  first_person = first_person,
  first_personp = first_personp,
  second_person = second_person,
  second_personp = second_personp,
  third_person = third_person,
  to_be = to_be,
  prepositions = prepositions
)