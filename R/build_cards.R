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

build_card <- function(str,
                       deck_format = build_deck()) {

  check_card_format(card = str, deck_format = deck_format)

  card <- list(
    chip_value = chip_value(str),
    eof = even_odd_face(str),
    suit = suit_of_card(str))

  structure(card, class = c("card", "list"))
}

build_card_set <- function(cards,
                        deck_format = build_deck()) {
  # browser()
  # if (length(cards == 1)) return(build_card(cards))

  card_set <- sapply(cards, build_card, simplify = FALSE)
  structure(card_set, class = c("card_set", "list"))
}

# Transform face cards and ace to numeric chip value
extract_digit <- function(str) {
  as.numeric(gsub("\\D", "", str))
}

face_to_chip <- function(x) gsub("[jqk]", "10", x)
ace_to_chip <- function(x) gsub("a", "11", x)

#' @export
chip_value <- function(x, ...) {
  UseMethod("chip_value", object = x)
}

#' @export
chip_value.default <- function(x, ...) {
  face_and_ace_as_num <- ace_to_chip(face_to_chip(x))
  add_class(extract_digit(face_and_ace_as_num), class_name = "chips")
}

#' @export
chip_value.card <- function(x, ...) {
  return(x$chip_value)
}

#' @export
chip_value.card_set <- function(x, ...) {
  add_class(sum(sapply(x, \(card) chip_value(card))),
            class_name = "chips")
}

is_debuffed <- function(card, debuff = NULL) {
  if (is.null(debuff)) return(FALSE)
  check_type(card, card_type = debuff)
}

#' @export
debuff <- function(x, debuff = NULL) {
  UseMethod("debuff")
}

#' @export
debuff.card <- function(x, debuff = NULL) {
  if (is_debuffed(x, debuff = debuff)) {
    x$chip_value <- 0
    x$eof <- NULL
    x$suit <- NULL
  }

  return(x)
}

#' @export
debuff.card_set <- function(x, debuff = NULL) {
  add_class(lapply(x, \(card) debuff(card, debuff = debuff)),
            class_name = "card_set")
}

even_odd_face <- function(card) {

  args <- as.list(environment())
  do.call(check_card_format, args)

  digit_or_ace <- grepl("^(\\d+|a)", card)
  if (!digit_or_ace) return("face")

  card <- ace_to_chip(card)
  card_num <- chip_value(card)
  if (card_num %% 2 == 0)
    return("even")
  return("odd")
}

suit_of_card <- function(card,
                         deck_format = build_deck()) {

  args <- as.list(environment())
  do.call(check_card_format, args)

  suit_format <- attr(deck_format, "suits")
  suit_keys <- get_attr_keys(deck_format, "suits")
  card_suit_key <- gsub(paste0("[^", suit_keys, "]"), "", card)

  suit_of_card <- names(suit_format)[suit_format == card_suit_key]
  return(suit_of_card)
}

#' @export
check_type <- function(card, card_type = NULL) {
  UseMethod("check_type")
}

#' @export
check_type.default <- function(card, card_type = NULL) {
  if (is.null(card_type)) return(TRUE)
  return(card_type %in% c(card$eof, card$suit))
}

#' @export
check_type.character <- function(card, card_type = NULL) {
  card <- build_card(card)
  NextMethod("check_type")
}

#' @export
count_types <- function(cards, card_type = NULL) {
  UseMethod("count_types")
}

#' @export
count_types.default <- function(cards, card_type = NULL) {
  sum(sapply(cards, \(card) check_type(card, card_type = card_type)))
}
