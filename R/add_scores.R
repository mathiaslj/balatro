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

# Chips
#' @export
chips <- function(x, card_type = NULL, foil = FALSE) {
  score_fun(x, "chips")
}

# Mult plus
#' @export
multp <- function(x, card_type = NULL) {
  score_fun(x, "multp", card_type = card_type)
}

# Mult x
#' @export
multx <- function(x, card_type = NULL) {
  score_fun(x, "multx", card_type = card_type)
}

# eval_score_funs <- function(score_funs, cards) {
#   lapply(score_funs, \(fun) fun(cards))
# }

#' @export
add_score <- function(x, balatro_score) {
  UseMethod("add_score")
}

add_chips <- function(x, balatro_score) {
  balatro_score$chips <- balatro_score$chips + x
  balatro_score
}

#' @export
add_score.chips <- function(x, balatro_score) {
  add_chips(x, balatro_score)
}

add_multp <- function(x, balatro_score) {
  balatro_score$mult <- balatro_score$mult + x
  balatro_score
}

#' @export
add_score.multp <- function(x, balatro_score) {
  add_multp(x, balatro_score)
}

add_multx <- function(x, balatro_score) {
  balatro_score$mult <- balatro_score$mult * x
  balatro_score
}

#' @export
add_score.multx <- function(x, balatro_score) {
  add_multx(x, balatro_score)
}

