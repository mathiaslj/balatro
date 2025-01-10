# Considerations:
# - balatro_score an R6 class?
# - instead of assigning cards with fx. "spades" class, what then about a
# "card" class with a field `suit = "spades"`?
#
# Things that don't work right now: Debuffs
balatro <- function(base_score = balatro_score(), cards, jokers, debuff = NULL,
                    ...,
                    deck_format = build_deck()) {
  cards <- match.arg(cards, choices = deck_format, several.ok = TRUE)

  bcards <- build_cards(cards)
  joker_vals <- lapply(jokers, \(fun) fun(cards))

  bscore <- add_card_score(bcards, base_score, debuff = debuff)
  for (j in joker_vals) {
    bscore <- add_score(j, bscore)
  }

  calc_balatro_score(bscore)
}
