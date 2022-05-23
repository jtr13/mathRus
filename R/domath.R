#' Start interactive math problems
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'   domath()
#' }
#'

domath <- function() {
  newprob <- NULL
  df <- googlesheets4::read_sheet("1stu5ekKxtUhwmc7GXk1lCslK-4gsL4t30W8qY-WfIkQ")
  choosefrom <- df$f[df$include=="yes"]
  usethis::ui_info("Welcome to mathRus!")
  usethis::ui_info("Answer \"s\" to skip a question.")
  while(is.null(newprob)) {
    cat("\n")
    index <- sample(length(choosefrom), 1)
    newprob <- match.fun(choosefrom[index])()
  }
}
