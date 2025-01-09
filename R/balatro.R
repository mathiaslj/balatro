balatro <- function(cards, jokers, ...,
                    deck_format = build_deck()) {
  cards <- match.arg(cards, choices = deck_format, several.ok = TRUE)
}
