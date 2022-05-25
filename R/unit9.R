# Unit 9
# add new functions to https://docs.google.com/spreadsheets/d/1stu5ekKxtUhwmc7GXk1lCslK-4gsL4t30W8qY-WfIkQ/edit#gid=0
#
#
#' What is the angle?
#'
#' Ex. Name a first quadrant angle with cos A = √2/2 .

#' @export

whats_the_angle <- function() {
  plot.new()
  refangles <- c(0, 30, 45, 60, 90)
  index <- sample(5, 1)
  quadnames <- c("first", "second", "third", "fourth")
  quadrant <- sample(4, 1)
  func <- sample(c("sin", "cos"), 1)
  ref <- refangles[index]
  angle <- switch(quadrant, ref, 180-ref, ref+180, 360-ref)
  allanswers <- c("0", "1/2", "√2/2", "√3/2", "1")
  answer <- ifelse(func == "sin", allanswers[index], allanswers[6-index])
  if (quadrant %in% c(2, 3) && func == "cos") answer <- paste("-", answer, sep = "")
  if (quadrant %in% c(3, 4) && func == "sin" ) answer <- paste("-", answer, sep = "")
  if (answer == "-0") answer <- "0"
  cat("Name a", quadnames[quadrant], "quadrant angle with", func, "A =", answer, ".\n")
  getresponse(angle)
}

#' Find another angle with the same sin, cos or tan
#'
#' Ex.Find an angle A between 0 and 360 degrees such that
#' sin A =  sin 55

#' @export

same_trig_func <- function() {
  angle <- sample(72,1)*5
  func <- sample(c("sin", "cos", "tan"), 1)
  cat("Find an angle A between 0 and 360 degrees such that\n")
  cat(paste(func, "A = ", func, angle, " "))
  solution <- get_other_angle(func, angle)
  x <- getresponse(solution)
  drawunitcircle()
  addangle(angle)
  addangle(solution, 2)
  return(x)
}

#' Unit circle practice
#'
#' Ex. What is cos 270 ?
#' Answer as a decimal with two decimal places.

#' @export
#'
sin_cos_tan <- function() {
  allangles <- c(seq(0, 330, 30), 45, 135, 225, 315)
  angle <- sample(allangles, 1) + sample(-2:2, 1)*360
  func <- sample(c("sin", "cos", "tan"), 1)
  cat("What is", func, angle, "?\n")
  cat("Answer as a decimal with two decimal places.\n")
  cat("Hint: √3/2 = .87; √2/2 = .71; √3 = 1.73; √3/3 = .58; enter 9999 for undefined.\n")
  solution <- round(match.fun(func)(angle*pi/180), 2)
  if (solution > 1000000) solution <- 9999
  x <- getresponse(solution)
  drawunitcircle()
  addangle(angle)
  return(x)
}

#' Practice with sin^2 X + cos^2 X = 1
#'
#' Ex. What is cos A, if sin A =  0.423 and
#' A is in Quadrant 1 ? Round to 2 decimal places.

#' @export

pythagorean_identity <- function() {
  angle <- sample(72,1)*5
  quad <- quadrant(angle)
  func <- sample(c("sin", "cos"), 1)
  otherfunc <- ifelse(func == "sin", "cos", "sin")
  value <- round(match.fun(func)(angle*pi/180), 3)
  cat("What is", otherfunc, "A, if", func, "A = ", value, "and\n")
  cat("A is in Quadrant", quadrant(angle), "? Round to 2 decimal places.\n")
  solution <- round(match.fun(otherfunc)(angle*pi/180), 2)
  x <- getresponse(solution)
  drawunitcircle()
  addangle(angle)
  return(x)
}

#' Inverse trig functions
#'
#' Ex. What is cos A, if sin A =  0.423 and
#' A is in Quadrant 1 ? Round to 2 decimal places.

#' @export

inverse_trig <- function() {
  angle <- sample(360, 1)
  allfunc <- c("sin", "cos", "tan")
  func <- sample(allfunc, 1)
  value <- round(match.fun(func)(angle*pi/180), 3)
  cat("Solve for 𝞱 (0 ≤𝞱≤ 360):", func, "𝞱 = ", value, "\n")
  cat("Round to the nearest integer.")
  sol1 <- round(match.fun(paste0("a", func))(value)*180/pi)
  if (sol1 < 0) sol1 <- sol1 + 360
  sol2 <- get_other_angle(func, sol1)
  x <- getresponse(c(sol1, sol2))
  drawunitcircle()
  addangle(sol1)
  addangle(sol2, 2)
  return(x)
}

#' sin A = cos B
#'
#' Ex. What is cos A, if sin A =  0.423 and
#' A is in Quadrant 1 ? Round to 2 decimal places.

#' @export
#'

sincos <- function() {
  angle <- sample(seq(0, 180, 5), 1)
  allfunc <- c("sin", "cos")
  func <- sample(allfunc, 1)
  otherfunc <- ifelse(func == "sin", "cos", "sin")
  sol1 <- ifelse(match.fun(func)(angle*pi/180) >= 0,
                 90 - ref_angle(angle),
                 270 - ref_angle(angle))
  sol2 <- get_other_angle(otherfunc, sol1)
  cat("Find A given", otherfunc, "A =", func, angle, "\n")
  getresponse(c(sol1, sol2))
}
