# NOTE: For jokers that give flat chips, mult (+ or X), individual funs will not
# be provided

# ride_the_bus <- function(mult) {
#   multp(mult)
# }
#
# gros_michel <- function(mult = 15) {
#   multp(mult)
# }

add_joker_value <- function(joker) {

}

build_joker <- function(val,
                        val_type = c("chips", "multp", "multx"),
                        card_type = NULL) {
  val_type <- match.arg(val_type)

}

odd_todd <- function(cards, chips_val = 31) {
  cards <- build_cards(cards)
  num_odds <- sum(sapply(cards, \(x) inherits(x, "odd")))
  return(chips_val * num_odds)
}

