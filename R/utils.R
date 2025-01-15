####################
# Classes
add_class <- function(x, class_name) {
  structure(x, class = c(class_name, class(x)))
}

# Transform face cards and ace to numeric chip value
extract_digit <- function(str) {
  as.numeric(gsub("\\D", "", str))
}

face_to_chip <- function(x) gsub("[jqk]", "10", x)
ace_to_chip <- function(x) gsub("a", "11", x)

face_to_rank <- function(x) {
  gsub("j", "11", x) %>%
    gsub("q", "12", .) %>%
    gsub("k", "13", .)
}
ace_to_rank <- function(x) gsub("a", "1", x)

foil_holo_poly <- function(foil = FALSE,
                           holographic = FALSE,
                           polychrome = FALSE) {
  args <- as.list(environment())
  if (sum(unlist(args)) > 1)
    cli::cli_abort("A card can be either {.arg foil}, {.arg holographic} or
                   {.arg polychrome}, but several are specified")

  if (foil) return(chips(50))
  if (holographic) return(multp(10))
  if (polychrome) return(multx(1.5))
}

