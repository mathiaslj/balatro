# IDEAS:
# - Use toupper to make this specification not sensitive to capitalization
# - Set names of cards using package level option

#' @export
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

#' @export
card <- function(str,
                       buff = NULL,
                       deck_format = build_deck()) {

  check_card_format(str, deck_format = deck_format)

  chips <- chips(str)
  score <- list(chips)
  if (!is.null(buff))
    score <- c(score, list(buff))

  card <- list(
    score = score,
    eof = even_odd_face(str),
    suit = suit_of_card(str),
    chip_count = as.numeric(chips),
    rank = rank(str),
    name = str)

  structure(card, class = c("card", "list"))
}

#' @export
card_set <- function(cards,
                           deck_format = build_deck()) {
  UseMethod("card_set")
}

#' @export
card_set.character <- function(cards,
                                     deck_format = build_deck()) {

  card_set <- sapply(cards, card, simplify = FALSE)
  card_set[[1]]$position <- "first"
  structure(card_set, class = c("card_set", "list"))
}

#' @export
card_set.card <- function(cards,
                                deck_format = build_deck()) {
  return(NULL)
}


  is_debuffed <- function(card, debuff = NULL) {
    if (is.null(debuff)) return(FALSE)
    check_type(card, card_trigger = debuff)
  }

  #' @export
  debuff <- function(x, debuff = NULL) {
    UseMethod("debuff")
  }

  #' @export
  debuff.card <- function(x, debuff = NULL) {
    if (is_debuffed(x, debuff = debuff)) {
      x$score <- chips(0)
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

  rank <- function(str) {
    face_and_ace_as_rank <- ace_to_rank(face_to_rank(str))
    return(extract_digit(face_and_ace_as_rank))
  }

  #' @export
  even_odd_face <- function(str) {

    check_card_format(str = str)

    digit_or_ace <- grepl("^(\\d+|a)", str)
    if (!digit_or_ace) return("face")

    card <- ace_to_chip(str)
    card_num <- as.numeric(chips(str))
    if (card_num %% 2 == 0)
      return("even")
    return("odd")
  }

  #' @export
  suit_of_card <- function(str,
                           deck_format = build_deck()) {

    args <- as.list(environment())
    do.call(check_card_format, args)

    suit_format <- attr(deck_format, "suits")
    suit_keys <- get_attr_keys(deck_format, "suits")
    card_suit_key <- gsub(paste0("[^", suit_keys, "]"), "", str)

    suit_of_card <- names(suit_format)[suit_format == card_suit_key]
    return(suit_of_card)
  }

  #' @export
  check_type <- function(card, card_trigger = NULL) {
    UseMethod("check_type")
  }

  #' @export
  check_type.default <- function(card, card_trigger = NULL) {
    if (is.null(card_trigger)) return(FALSE)

    trigger_matches_card <- any(card_trigger %in%
                                  c(card$eof, card$suit, card$rank, card$position, card$name))
    return(trigger_matches_card)
  }

  #' @export
  check_type.character <- function(card, card_trigger = NULL) {
    card <- card(card)
    NextMethod("check_type")
  }

  #' #' @export
  #' count_types <- function(cards, card_trigger = NULL) {
  #'   sum(sapply(cards, \(card) check_type(card, card_trigger = card_trigger)))
  #' }
