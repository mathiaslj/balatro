# Return function of cards that take a set of cards and return the value
# it adds to chips, multp or multx
#' @export
score_fun <- function(x, score_type, card_type = NULL) {
  if (missing(x))
    cli::cli_abort("{.var x} is missing with no default")
  if (missing(score_type))
    cli::cli_abort("{.var score_type} is missing with no default")

  if (is.null(card_type)) {
    return(function(cards) add_class(x, score_type))
  }

  function(cards) add_class(x * count_types(cards, card_type), score_type)
}

#### NEW
#' @export
score_class <- function(x, class, card_type = NULL) {
  UseMethod("score_class")
}

#' @export
score_class.default <- function(x, class, card_type = NULL) {
  out <- add_class(x, class_name = class)
  if (!is.null(card_type))
    out <- structure(add_class(out, class_name = "trigger"),
                     trigger_type = card_type)

  return(out)
}

#' @export
score_class.character <- function(x, class, card_type = NULL) {
  face_and_ace_as_num <- ace_to_chip(face_to_chip(x))
  x <- extract_digit(face_and_ace_as_num)
  NextMethod("score_class")
}

#' #' @export
#' chip_value.card <- function(x, ...) {
#'   return(x$chip_value)
#' }
#'
#' #' @export
#' chip_value.card_set <- function(x, ...) {
#'   chips(sum(sapply(x, \(card) chip_value(card))))
#' }

# Chips
#' @export
# chips <- function(x, card_type = NULL) {
#   score_fun(x, "chips", card_type = card_type)
# }

chips <- function(x, card_type = NULL) {
  score_class(x, class = "chips", card_type = card_type)
}

# Mult plus
#' @export
# multp <- function(x, card_type = NULL) {
#   score_fun(x, "multp", card_type = card_type)
# }

multp <- function(x, card_type = NULL) {
  score_class(x, class = "multp", card_type = card_type)
}

# Mult x
#' @export
# multx <- function(x, card_type = NULL) {
#   score_fun(x, "multx", card_type = card_type)
# }

multx <- function(x, card_type = NULL) {
  score_class(x, class = "multx", card_type = card_type)
}

#' @export
add_to_card <- function(x, ...) {
  UseMethod("add_to_card")
}

#' #' @export
#' add_to_card.default <- function(x, trigger) {
#'   c(x, trigger)
#' }

#' @export
add_to_card.card <- function(x, trigger) {
  x$score <- c(x$score, list(trigger))
  x
}

retrigger <- function(...) {
  invisible()
}



