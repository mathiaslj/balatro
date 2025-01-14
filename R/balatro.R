# Considerations:
# - balatro_score an R6 class?
# - instead of assigning cards with fx. "spades" class, what then about a
# "card" class with a field `suit = "spades"`?
#
# Things that don't work right now: Debuffs

#' Calculate a balatro score
#'
#' Scores are generated in the order of operations:
#' - Base score based on level of hand
#'   - Specify with `base_score` argument
#' - Chip count from cards are added
#'   - Taken from the specification of
#' `cards`
#' - Chip and mult buffs from cards are added in the order
#'   1) played cards,
#'   2) cards held in hand
#'   - Specify these using `hand_buffs` (make sure to order them correctly
#'   according to left to right of played cards and held cards)
#' - Chip and mult from jokers
#'   - Specify using `jokers` argument
#'   - Foil (`chips`), holographic (`multp`) and polychrome (`multx`)
#' should also just be passed on in this argument
#'
#' @param base_score a `balatro_score`
#' @param cards a `list` of `character`s and `card`s (see examples)
#' @param jokers a `vector` or `list` of calls to `chips`, `multp`
#' or `multx`
#' and multx functions
#' @param debuff a `character` specifying what type of cards are debuffed.
#' Fx. `debuff = "odd"`, `debuff = "hearts"`, etc.
#' @param hand_buffs like `joker` a collection of calls to
# `chips`, `multp` or `multx` from playing cards and cards in hand
#' @param ... Nothing for now
#' @param deck_format the result of a call to `build_deck`. At the moment,
#' the default call to `build_deck` builds a deck with an assumed format
#' that is needed for other functionalities. Do not touch this argument.
#'
#' @details
#' Use fx. `debuff = "hearts"` to debuff hearts so they produce no chip
#' value and do not have any characteristic towards a joker's scoring.
#' Fx. if you have a joker that gives mult to even cards but you have a
#' 10 of hearts with hearts debuffed, that 10 won't give mult from the joker
#'
#' @returns a `numeric` with the score
#' @export
#'
#' @examples
#' # Checking the value of a level 1 "flush" with no jokers
#' balatro(base_score = balatro_score(chips = 35, mult = 4),
#'         cards = list("qc", "9c", "8c", "4c", "2c"))
#'
#' # Level 3 "three of a kind" with odd_todd joker that gives 31 chips
#' # for odd ranked cards and 4+ mult on 2 of the playing cards while
#' # clubs are debuffed
#' balatro(base_score = balatro_score(chips = 70, mult = 7),
#'         cards = list("7d", "7d", "7c"),
#'         jokers = list(chips(31, card_trigger = "odd")),
#'         hand_buffs = list(multp(2*4)),
#'         debuff = "clubs")
#'
#' # Level 14 "five of a kind" with 10s with jokers adding their bonuses
#' # up to 118 +mult and 1.5*2.8*2.2*3*3 Xmult. All played cards are glass
#' # cards, giving 2X mult, and 3 steel cards are held in hand, each giving
#' # 1.5X mult
#' balatro(base_score = balatro_score(chips = 575, mult = 51),
#'         cards = c(rep(list("10d"), 4), "10h"),
#'         jokers = list(multp(118), multx(1.5*2.8*2.2*3*3)),
#'         hand_buffs = list(multx(2^5), multx(1.5^3)))
#'
#' # NEW EXAMPLES
#' balatro(base_score = balatro_score(chips = 35, mult = 4),
#'         cards = list("qc", "9c", "8c", "4c", "2c"),
#'         jokers = list(multx(2, "qc"),
#'                       retrigger(2, "first")))
#'
#' balatro(base_score = balatro_score(chips = 35, mult = 4),
#'         cards = list(card("qc", buff = list(multx(2), retrigger(1))),
#'                      "9c", "8c", "4c", "2c"),
#'         jokers = list(idol("qc"),
#'                       hanging_chad(),
#'                       walkie_talkie(),
#'                       odd_todd()),
#'          hand_buffs = list(steel(2)))
balatro <- function(cards,
                    base_score = balatro_score(),
                    jokers = NULL,
                    hand_buffs = NULL,
                    debuff = NULL,
                    ...,
                    deck_format = build_deck()) {
  if (!is.null(jokers)) {
    checkmate::assert_list(jokers)
    jokers <- purrr::list_flatten(jokers)
  }
  if (!is.null(hand_buffs)) {
    checkmate::assert_list(hand_buffs)
    hand_buffs <- purrr::list_flatten(hand_buffs)
  }

  checkmate::assert_list(cards)
  card_set <- card_set(cards)
  card_set <- debuff(card_set, debuff = debuff)

  # TODO: Fix so this match.arg works no matter if a string or card_set
  # is given
  # cards <- match.arg(cards, choices = deck_format, several.ok = TRUE)

  trig_ind <- sapply(jokers, \(x) !is.null(attr(x, "card_trigger")))
  reg_jokers <- jokers[!trig_ind]
  trigger_jokers <- jokers[trig_ind]

  n_trigger <- length(trigger_jokers)
  if (n_trigger > 0) {
    for (i in 1:n_trigger) {
      card_set <- add_to_card(card_set, trigger_jokers[[i]])
    }
  }
  card_set <- resolve_retriggers(card_set)

  card_set_scores <- sapply(card_set, \(x) x$score)
  scores_to_add <- c(card_set_scores, hand_buffs, reg_jokers)
  scores_to_add_f <- Filter(\(x) !is.null(x) && length(x) > 0, scores_to_add)

  score <- base_score
  for (k in scores_to_add_f) {
    score <- add_score(k, score)
  }

  format(calc_balatro_score(score), big.mark = ",")
}
