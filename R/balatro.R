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
#' @returns
#' @export
#'
#' @examples
balatro <- function(base_score = balatro_score(), cards, jokers, debuff = NULL,
                    ...,
                    deck_format = build_deck()) {
  cards <- match.arg(cards, choices = deck_format, several.ok = TRUE)

  browser()
  bcards <- build_card_set(cards)
  joker_vals <- lapply(jokers, \(fun) fun(bcards))

  bscore <- add_card_score(bcards, base_score, debuff = debuff)
  for (j in joker_vals) {
    bscore <- add_score(j, bscore)
  }

  calc_balatro_score(bscore)
}
