# Helper for check_card_correct_format to use values with regex
get_attr_keys <- function(x, attr_name) {
  attr_format <- attr(x, attr_name)
  paste(attr_format, collapse = "")
}

# Check if card format is correct
#' @export
check_card_format <- function(
    str,
    deck_format = build_deck()) {

  card_str <- match.arg(str, choices = deck_format)

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
    cli::cli_abort("Provide a card {.var str} of correct format")

  return(invisible())
}
