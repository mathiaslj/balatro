balatro_score <- function(chips = 10, mult = 4) {
  vals <- as.list(environment())
  structure(vals, class = "balatro_score")
}

calc_balatro_score <- function(balatro_score) {
  balatro_score$chips * balatro_score$mult
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
#     add_chips = function(x) {
#       self$chips <- self$chips + x
#       return(invisible(self))
#     },
#     add_mult = function(x) {
#       self$mult <- self$mult + x
#       return(invisible(self))
#     },
#     times_mult = function(x) {
#       self$mult <- self$mult * x
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
