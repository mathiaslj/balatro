add_class <- function(x, class_name) {
  structure(x, class = c(class_name, class(x)))
}

# Chips
chips <- function(x, foil = FALSE) {
  add_class(x, "chips")
}

# Mult plus
multp <- function(x) {
  add_class(x, "multp")
}

# Mult x
multx <- function(x) {
  add_class(x, "multx")
}
