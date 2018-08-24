#' FO: delay_by
#'
#' This FO can make function sleep n seconds before operating.
#'
#' @param delay The delay seconds.
#' @param f The base function.
#' @examples
#' system.time(delay_by(1,runif)(100))
#'
#' @export
delay_by <- function(delay, f){
  function(...) {
    Sys.sleep(delay)
    f(...)
  }
}

#' FO: chatty
#'
#' This FO can print the first parameter when processing.
#'
#' @examples
#' f <- function(x) x^2
#' chatty(f)(5)
#'
#' s <- c(3,2,1)
#' vapply(s, chatty(f), numeric(1))
#'
#' @export
chatty <- function(f) {
  function(x, ...) {
    res <- f(x, ...)
    cat("Processing ", x, "\n", sep = "")
    res
  }
}

#' FO: dot_every
#'
#' This FO can print a dot when every n times operation.
#'
#' @param n The n times operation.
#' @param f The base fucntion.
#'
#' @examples
#' x <- lapply(1:100, dot_every(10, runif))
#' @export
dot_every <- function(n, f) {
  i <- 1
  function(...) {
    if (i %% n == 0) cat(".")
    i <<- i+1
    f(...)
  }
}

#' FO: failwith
#'
#' This FO can return a default value when Error occurs.
#'
#' @examples
#' failwith(NA,log)("a")
#'
#' failwith(NA,log,quiet = T)("a")
#'
#' @export
failwith <- function(default = NULL, f, quiet = FALSE) {
  force(f)
  function(...) {
    out <- default
    try(out <- f(...), silent = quiet)
    out
  }
}

#' FO: fcosttime
#'
#' This FO can print the operating time of function before print the outcome.
#'
#' @examples
#' fcosttime(delay_by(10,sum))(1:100)
#' @export
fcosttime <- function(f) {
  function(...) {
    costtime <- system.time(out <- f(...))
    cat("Function operates for",costtime[3],"seconds.","\n","\n")
    out
  }
}

#' FO: and
#'
#' This FO can realize logical and.
#'
#' @export
and <- function(f1, f2) {
  force(f1); force(f2)
  function(...) {
    f1(...) && f2(...)
  }
}

#' FO: and
#'
#' This FO can realize logical or.
#'
#' @export
or <- function(f1, f2) {
  force(f1); force(f2)
  function(...) {
    f1(...) || f2(...)
  }
}

#' FO: and
#'
#' This FO can realize logical not.
#'
#' @export
not <- function(f) {
  force(f)
  function(...) {
    !f(...)
  }
}
