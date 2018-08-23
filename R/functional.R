#' Logical functional: where
#'
#' Return a logical vector to judge every elements in list.
#'
#' @param f A function returns logical value.
#' @param x The list.
#' @examples
#' df <- data.frame(x = 1:3, y = c("a","b","c"))
#' where(is.factor,df)
#'
#' @export
where <- function(f, x) {
  vapply(x, f, logical(1))
}

#' Functional: compact
#'
#' This functional can reserve elements not null in list.
#'
#' @param l The object list.
#'
#' @examples
#' x <- list(a = 1:10,b=NULL,c=letters[1:10])
#' compact(x)
#'
#' @export
compact <- function(l) {
  Filter(Negate(is.null),l)
}

#' FO: fcosttime
#'
#' This FO can print the operating time of function before print the outcome.
#'
#' @examples
#' fcosttime(delay_by(10,sum))(1:100)
#'
fcosttime <- function(f) {
  function(...) {
    costtime <- system.time(out <- f(...))
    cat("Function operates for",costtime[3],"seconds.","\n","\n")
    out
  }
}
