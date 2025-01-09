# IDEAS:
# - Use toupper to make this specification not sensitive to capitalization
# - Set names of cards using package level option

build_deck <- function(suits = c("s", "c", "d", "h"),
                       face_cards = c("j", "q", "k"),
                       ace = "a",
                       num_cards = 2:10) {

  card_vals <- c(num_cards, ace, face_cards)
  suit_sets <- lapply(suits, \(x) paste(card_vals, x, sep = ""))
  deck <- unlist(suit_sets)

  out <- structure(deck,
                   suits = suits, face_cards = face_cards,
                   ace = ace, num_cards = num_cards)
  return(deck)
}

build_card <- function(str,
                       deck_format = build_deck()) {
  card <- match.arg(str, choices = deck_format)
  card_face_to_num <- face_to_chip(card)

  suits <- attr(deck_format, "suits")
  suits_str <- paste(suits, collapse = "")
  card_match_regex <- paste0("^(1[01]|[1-9])[", suits_str, "]$")

  card_correct_format <- grepl(card_match_regex, card_face_to_num)
  if (!card_correct_format)
    cli::cli_abort("Provide a card {.var str} of correct format")


}

face_to_chip <- function(card, face_to_chip_map = list(jqk = 10, a = 11)) {
  browser()

  trans_face <- function(x) gsub("[jqk]", "10", x)
  trans_ace <- function(x) gsub("a", "11", x)

  out <- trans_ace(trans_face(card))
  return(out)
}

assign_suit <- function(x, suit) {
  attr(x, "suit") <- suit
  return(x)
}

spades <- function(x, regex = "s$") {

}
