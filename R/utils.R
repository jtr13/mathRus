#' @importFrom graphics abline text arrows

quadrant <- function(angle) {
  angle <- angle %% 360
  floor(angle/90+1)
}


sinsol <- function(angle) {
  dplyr::case_when(
    quadrant(angle) <= 2 ~ 180 - angle,
    TRUE ~ 540 - angle
  )
}

cossol <- function(angle) 360 - angle

drawunitcircle <- function() {
  theta <- seq(0, 2*pi, length.out = 200)
  x <- cos(theta); y <- sin(theta)
  plot(x, y, asp = 1, type = "l", las = 1, xlim <- c(-1.5, 1.5), ylim = c(-1.5, 1.5))
  abline(h = 0)
  abline(v = 0)
}

addangle <- function(angle, color = 1, label = TRUE) {
  colors <- c("red", "blue")
  x <- cos(angle*pi/180)
  y <- sin(angle*pi/180)
  arrows(0, 0, x, y, length = .1, col = colors[color])
  if (label) text(1.25*x, 1.25*y, angle, col = colors[color])
}

getresponse <- function(solution) {
  ans <- readline()
  if (is.na(ans) || suppressWarnings(is.na(as.numeric(ans)))) {
    alldone()
    return(-1)
    } else {
    if(as.numeric(ans) == solution) {
      usethis::ui_done("Correct!")
      } else {
      usethis::ui_oops(paste("Incorrect. The answer is:", solution))
      }
    }
}

alldone <- function() {
  cat("Good-bye Zupie! Hope to see you soon.\n")
}
