# NOTE: For jokers that give flat chips, mult (+ or X), individual funs will not
# be provided

# ride_the_bus <- function(mult) {
#   multp(mult)
# }
#
# gros_michel <- function(mult = 15) {
#   multp(mult)
# }

steel <- function(n = 1, red_seals = 0) {
  multx(1.5^(n+red_seals))
}

baron <- function(n_kings = 1, red_seals = 0) {
  steel(n_kings, red_seals)
}

odd_todd <- function(...) {
  chips(31, card_trigger = "odd", ...)
}

hanging_chad <- function(...) {
  retrigger(2, "first", ...)
}

walkie_talkie <- function(...) {
  list(chips(10, card_trigger = c(4,10), ...),
       multp(4, card_trigger = c(4,10)))
}

# fx idol("2h")
idol <- function(str, ...) {
  multx(2, card_trigger = str, ...)
}
