# Helper for check_card_correct_format to use values with regex
get_attr_keys <- function(x, attr_name) {
  attr_format <- attr(x, attr_name)
  paste(attr_format, collapse = "")
}

# Check if card format is correct
# card argument is always a string right now, but naming as card to make error
# message from match.arg
#' @export
check_card_format <- function(
    str,
    deck_format = build_deck()) {

  suit_keys <- get_attr_keys(deck_format, "suits")
  facecard_keys <- get_attr_keys(deck_format, "face_cards")
  ace_key <- attr(deck_format, "ace")

  card_match_regex <- paste0(
    "^([1-9]|10|[",
    facecard_keys,
    ace_key,
    "])[", suit_keys, "]$")

  card_correct_format <- grepl(card_match_regex, str)
  if (!card_correct_format)
    cli::cli_abort("Provide a card of correct format")

  return(invisible())
}
