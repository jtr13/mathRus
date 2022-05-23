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

#' Find another angle with the same sin or cos
#'
#' Ex.Find an angle A between 0 and 360 degrees such that
#' sin A =  sin 55

#' @export

same_sin_cos <- function() {
  angle <- sample(72,1)*5
  func <- sample(c("sin", "cos"), 1)
  cat("Find an angle A between 0 and 360 degrees such that\n")
  cat(paste(func, "A = ", func, angle, " "))
  drawunitcircle()
  addangle(angle)
  if(func == "sin") {
    solution <- ifelse(quadrant(angle) <=2, 180 - angle, 540 - angle)
    } else {
    solution <- 360 - angle
    }
  x <- getresponse(solution)
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
  drawunitcircle()
  addangle(angle, label = FALSE)
  quad <- quadrant(angle)
  func <- sample(c("sin", "cos"), 1)
  otherfunc <- ifelse(func == "sin", "cos", "sin")
  value <- round(match.fun(func)(angle), 3)
  cat("What is", otherfunc, "A, if", func, "A = ", value, "and\n")
  cat("A is in Quadrant", quadrant(angle), "? Round to 2 decimal places.\n")
  solution <- round(ifelse(func == "sin", cos(angle*pi/180), sin(angle*pi/180)), 2)
  getresponse(solution)
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
  sol2 <- switch(which(allfunc == func),
                      switch(quadrant(sol1), 180-sol1, 180-sol1, 540-sol1, 540-sol1),
                      360 - sol1,
                      switch(quadrant(sol1), 180+sol1, 180+sol1, sol1-180, sol1-180))
  if (sol1 != sol2) {
    getresponse(c(sol1, sol2))
  } else {
    getresponse(sol1)
  }
}

