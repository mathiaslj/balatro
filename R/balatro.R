# Considerations:
# - balatro_score an R6 class?
# - instead of assigning cards with fx. "spades" class, what then about a
# "card" class with a field `suit = "spades"`?
#
# Things that don't work right now: Debuffs

#' Title
#'
#' @param base_score a `balatro_score`
#' @param cards a `character` vector of card names
#' @param jokers a `vector` or `list` of function created with chips, multp
#' and multx functions
#' @param debuff a `character` specifying what type of cards are debuffed
#' @param ... Nothing for now
#' @param deck_format a `character`
#'
#' @returns a `numeric` with the score
#' @export
#'
#' @examples
#' balatro(cards = c("2h", "jc"), jokers = list(multp(10)), debuff = "even")
balatro <- function(base_score = balatro_score(), cards, jokers, debuff = NULL,
                    ...,
                    deck_format = build_deck()) {
  cards <- match.arg(cards, choices = deck_format, several.ok = TRUE)
  checkmate::assert_list(jokers)

  card_set <- build_card_set(cards)
  cards_chip_value <- chip_value(card_set, debuff = debuff)
  joker_vals <- lapply(jokers, \(fun) fun(card_set))

  score <- base_score
  score <- add_score(cards_chip_value, score)
  for (j in joker_vals) {
    score <- add_score(j, score)
  }

  calc_balatro_score(score)
}
