add_card_score <- function(x) {
  sum(unlist(x))
}

balatroScore <- R6::R6Class(
  "balatroScore",
  public = list(
    initialize = function(chips, multp, multx) {
      self$chips <- chips
      self$multp <- multp
      self$multx <- multx
    },
    chips = NULL,
    multp = NULL,
    multx = NULL
  ),
  active = list(
    score = function() self$chips * (self$multp * self$multx)
  )
)

a <- balatroScore$new(10, 10, 10)
a$score
