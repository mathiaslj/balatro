# IDEAS:
# - Use toupper to make this specification not sensitive to capitalization
# - Set names of cards using package level option

build_deck <- function(suits = c(spades = "s",
                                 clubs = "c",
                                 diamonds = "d",
                                 hearts = "h"),
                       face_cards = c(jack = "j", queen = "q", king = "k"),
                       ace = "a",
                       num_cards = 2:10) {

  # suits <- toupper(suits)
  # face_cards <- toupper(suits)
  # ace <- toupper(ace)

  card_vals <- c(num_cards, face_cards, ace)
  suit_sets <- lapply(suits, \(x) paste(card_vals, x, sep = ""))
  deck <- unname(unlist(suit_sets))

  out <- structure(deck,
                   suits = suits, face_cards = face_cards,
                   ace = ace, num_cards = num_cards)
  return(out)
}

build_card <- function(card,
                       deck_format = build_deck()) {

  args <- as.list(environment())
  do.call(check_card_correct_format, args)

  card %>%
    add_class(class_name = even_odd_face(.)) %>%
    add_class(class_name = suit_of_card(.)) %>%
    chip_value()
}

build_cards <- function(cards,
                        deck_format = build_deck()) {
  # browser()
  # if (length(cards == 1)) return(build_card(cards))

  return(sapply(cards, build_card, simplify = FALSE))
}

# Helper for check_card_correct_format to use values with regex
get_attr_keys <- function(x, attr_name) {
  attr_format <- attr(x, attr_name)
  paste(attr_format, collapse = "")
}

# Check if card format is correct
check_card_correct_format <- function(
    card,
    deck_format = build_deck()) {

  card_str <- match.arg(card, choices = deck_format)

  suit_keys <- get_attr_keys(deck_format, "suits")
  facecard_keys <- get_attr_keys(deck_format, "face_cards")
  ace_key <- attr(deck_format, "ace")

  card_match_regex <- paste0(
    "^([1-9]|10|[",
    facecard_keys,
    ace_key,
    "])[", suit_keys, "]$")

  card_correct_format <- grepl(card_match_regex, card)
  if (!card_correct_format)
    cli::cli_abort("Provide a card {.var card} of correct format")

  return(invisible())
}

# Transform face cards and ace to numeric chip value
trans_face <- function(x) gsub("[jqk]", "10", x)
trans_ace <- function(x) gsub("a", "11", x)

face_to_chip <- function(card, face_to_chip_map = list(jqk = 10, a = 11)) {
  out <- trans_ace(trans_face(card))
  return(out)
}

chip_value <- function(card, keep_classes = TRUE) {
  card_num <- face_to_chip(card)
  digit <- as.numeric(gsub("\\D", "", card_num))
  if (!keep_classes) return(digit)

  classes <- class(card)
  classes <- classes[classes != "character"]
  classes <- c(classes, "numeric")
  out <- structure(digit, class = classes)
  return(out)
}

even_odd_face <- function(card) {

  args <- as.list(environment())
  do.call(check_card_correct_format, args)

  digit_or_ace <- grepl("^(\\d+|a)", card)
  if (!digit_or_ace) return("face")

  card <- trans_ace(card)
  card_num <- chip_value(card)
  if (card_num %% 2 == 0)
    return("even")
  return("odd")
}

suit_of_card <- function(card,
                 deck_format = build_deck()) {

  args <- as.list(environment())
  do.call(check_card_correct_format, args)

  suit_format <- attr(deck_format, "suits")
  suit_keys <- get_attr_keys(deck_format, "suits")
  card_suit_key <- gsub(paste0("[^", suit_keys, "]"), "", card)

  suit_of_card <- names(suit_format)[suit_format == card_suit_key]
  return(suit_of_card)
}

count_types <- function(cards, card_type = NULL) {
  if (is.null(card_type)) return(length(cards))
  sum(sapply(cards, \(x) inherits(x, card_type)))
}
