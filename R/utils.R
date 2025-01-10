####################
# Classes
add_class <- function(x, class_name) {
  structure(x, class = c(class_name, class(x)))
}
