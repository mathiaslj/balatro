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

retrigger_to_back <- function(list) {
  retrig_ind <- sapply(list, \(x) inherits(x, "retrigger"))
  out <- c(list[!retrig_ind], list[retrig_ind])
  return(out)
}
