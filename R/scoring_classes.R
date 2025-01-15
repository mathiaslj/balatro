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
score_class <- function(x, class, card_trigger = NULL,
                        foil = FALSE, holographic = FALSE,
                        polychrome = FALSE) {
  UseMethod("score_class")
}

#' @export
score_class.default <- function(x, class, card_trigger = NULL,
                                foil = FALSE, holographic = FALSE,
                                polychrome = FALSE) {
  score <- structure(add_class(x, class_name = class),
                     card_trigger = card_trigger)

  if (any(c(foil, holographic, polychrome))) {
    score <- list(
      score,
      foil_holo_poly(
        foil = foil,
        holographic = holographic,
        polychrome = polychrome
      )
    )
  }

  return(score)
}

#' @export
score_class.character <- function(x, class, card_trigger = NULL,
                                  foil = FALSE, holographic = FALSE,
                                  polychrome = FALSE) {
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

chips <- function(x, card_trigger = NULL,
                  foil = FALSE, holographic = FALSE,
                  polychrome = FALSE) {
  args <- as.list(environment())
  do.call(score_class, c(list(class = "chips"), args))
}

# Mult plus
#' @export
# multp <- function(x, card_trigger = NULL) {
#   score_fun(x, "multp", card_trigger = card_trigger)
# }

multp <- function(x, card_trigger = NULL,
                  foil = FALSE, holographic = FALSE,
                  polychrome = FALSE) {
  args <- as.list(environment())
  do.call(score_class, c(list(class = "multp"), args))
}

# Mult x
#' @export
# multx <- function(x, card_trigger = NULL) {
#   score_fun(x, "multx", card_trigger = card_trigger)
# }

multx <- function(x, card_trigger = NULL,
                  foil = FALSE, holographic = FALSE,
                  polychrome = FALSE) {
  args <- as.list(environment())
  do.call(score_class, c(list(class = "multx"), args))
}

#' @export
retrigger <- function(x, card_trigger = NULL,
                      foil = FALSE, holographic = FALSE,
                      polychrome = FALSE) {
  args <- as.list(environment())
  do.call(score_class, c(list(class = "retrigger"), args))
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

  x$score <- c(x$score, list(trigger))

  return(x)
}

#' @export
add_to_card.card_set <- function(x, trigger) {
  add_class(lapply(x, \(card) add_to_card(card, trigger)),
            class_name = "card_set")
}

#' @export
resolve_retriggers <- function(x) {
  UseMethod("resolve_retriggers")
}

#' @export
resolve_retriggers.card <- function(x) {
  scores <- x$score

  retriggers <- sapply(
    scores,
    \(x) if (inherits(x, "retrigger")) x else 0
  )
  is_retrigger <- as.logical(retriggers)
  ind_not_retrigger <- which(!is_retrigger)

  score_no_retrigger <- scores[ind_not_retrigger]
  n_retriggers <- sum(unlist(retriggers))

  x$score <- rep(score_no_retrigger, 1 + n_retriggers)
  return(x)
}

#' @export
resolve_retriggers.card_set <- function(x) {
  add_class(lapply(x, \(x) resolve_retriggers(x)),
            class_name = "card_set")
}
