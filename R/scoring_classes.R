# Return function of cards that take a set of cards and return the value
# it adds to chips, multp or multx
#' @export
score_fun <- function(x, score_type, card_trigger = NULL) {
  if (missing(x))
    cli::cli_abort("{.var x} is missing with no default")
  if (missing(score_type))
    cli::cli_abort("{.var score_type} is missing with no default")

  if (is.null(card_trigger)) {
    return(function(cards) add_class(x, score_type))
  }

  function(cards) add_class(x * count_types(cards, card_trigger), score_type)
}

#### NEW
#' @export
score_class <- function(x, class, card_trigger = NULL) {
  UseMethod("score_class")
}

#' @export
score_class.default <- function(x, class, card_trigger = NULL) {
  structure(add_class(x, class_name = class),
            card_trigger = card_trigger)
}

#' @export
score_class.character <- function(x, class, card_trigger = NULL) {
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
# chips <- function(x, card_trigger = NULL) {
#   score_fun(x, "chips", card_trigger = card_trigger)
# }

chips <- function(x, card_trigger = NULL) {
  score_class(x, class = "chips", card_trigger = card_trigger)
}

# Mult plus
#' @export
# multp <- function(x, card_trigger = NULL) {
#   score_fun(x, "multp", card_trigger = card_trigger)
# }

multp <- function(x, card_trigger = NULL) {
  score_class(x, class = "multp", card_trigger = card_trigger)
}

# Mult x
#' @export
# multx <- function(x, card_trigger = NULL) {
#   score_fun(x, "multx", card_trigger = card_trigger)
# }

multx <- function(x, card_trigger = NULL) {
  score_class(x, class = "multx", card_trigger = card_trigger)
}

#' @export
retrigger <- function(x, card_trigger = NULL) {
  score_class(x, class = "retrigger", card_trigger = card_trigger)
}


#' Jokers that trigger on certain cards are added as scoring to cards
#'
#' @param x object to dispatch on
#' @param trigger a `chips`, `multp` or `multx` object
#'
#' @export
add_to_card <- function(x, trigger) {
  UseMethod("add_to_card")
}

#' #' @export
#' add_to_card.default <- function(x, trigger) {
#'   c(x, trigger)
#' }

#' @export
add_to_card.card <- function(x, trigger) {
  trigger_matches_card <- check_type(
    x, card_trigger = attr(trigger, "card_trigger")
  )
  if (!trigger_matches_card) return(x)

  is_trigger_retrigger <- inherits(trigger, "retrigger")
  if (is_trigger_retrigger)
    x$score <- rep(x$score, 1 + as.numeric(trigger))
  else
    x$score <- c(x$score, list(trigger))

  return(x)
}

#' @export
add_to_card.card_set <- function(x, trigger) {
  add_class(lapply(x, \(card) add_to_card(card, trigger)),
            class_name = "card_set")
}


