#' @export
balatro_score <- function(chips = 10, mult = 4) {
  vals <- as.list(environment())
  structure(vals, class = "balatro_score")
}

#' @export
calc_balatro_score <- function(balatro_score) {
  as.numeric(balatro_score$chips * balatro_score$mult)
}

# balatroScore <- R6::R6Class(
#   "balatroScore",
#   public = list(
#     initialize = function(chips, mult) {
#       self$chips <- chips
#       self$mult <- mult
#     },
#     chips = NULL,
#     mult = NULL,
#     add_score = function(x) {
#       self$chips <- self$chips + x
#       return(invisible(self))
#     }
#   ),
#   active = list(
#     score = function() self$chips * self$mult
#   )
# )
#
# a <- balatroScore$new(10, 10)
# a$score

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
