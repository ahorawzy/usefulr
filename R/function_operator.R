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
