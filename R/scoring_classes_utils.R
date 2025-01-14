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
