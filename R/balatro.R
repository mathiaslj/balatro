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
#' @param buffs like `joker` a collection of chips, multp or multx fx. from
#' @param ... Nothing for now
#' @param deck_format a `character`
#'
#' @returns a `numeric` with the score
#' @export
#'
#' @examples
#' balatro(cards = c("2h", "jc"), jokers = list(multp(10)), debuff = "even")
balatro <- function(cards, jokers,
                    base_score = balatro_score(),
                    debuff = NULL,
                    buffs = NULL,
                    ...,
                    deck_format = build_deck()) {
  cards <- match.arg(cards, choices = deck_format, several.ok = TRUE)
  checkmate::assert_list(jokers)

  card_set <- debuff(build_card_set(cards), debuff = debuff)

  cards_chip_value <- chip_value(card_set)
  joker_values <- lapply(jokers, \(fun) fun(card_set))

  scores_to_add <- c(list(cards_chip_value), joker_values)

  score <- base_score
  score <- add_score(cards_chip_value, score)
  for (j in joker_values) {
    score <- add_score(j, score)
  }

  calc_balatro_score(score)
}
