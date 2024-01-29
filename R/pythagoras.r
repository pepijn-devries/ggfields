#' A helper function to calculate vector lengths
#' 
#' Calculates the length of a vector using the Pythagorean theorem.
#' @param x A `numeric` `vector` with the same length as `y`. It should
#' represent the lengths of the first leg (cathetus) of right triangles.
#' @param y A `numeric` `vector` with the same length as `x`. It should
#' represent the lengths of the second leg (cathetus) of right triangles.
#' @returns Returns a `numeric` `vector` with the same length as `x` and `y`,
#' reflecting the lengths of the hypotenuse of the right triangles.
#' @examples
#' pythagoras(x = c(1, 2), y = c(1, 2))
#' @author Pepijn de Vries
#' @export
pythagoras <- function(x, y) {
  return(sqrt(x^2 + y^2))
}