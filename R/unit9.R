# Unit 9
# add new functions to https://docs.google.com/spreadsheets/d/1stu5ekKxtUhwmc7GXk1lCslK-4gsL4t30W8qY-WfIkQ/edit#gid=0

same_sin_cos <- function() {
  angle <- sample(72,1)*5
  func <- sample(c("sin", "cos"), 1)
  cat("Find an angle A between 0 and 360 degrees such that\n")
  cat(paste(func, "A = ", func, angle, " "))
  drawunitcircle()
  addangle(angle)
  solution <- ifelse(func == "sin", sinsol(angle), cossol(angle))
  getresponse(solution)
  addangle(solution, 2)
}

sin_cos_tan_0to360 <- function() {
  allangles <- c(seq(0, 330, 30), 45, 135, 225, 315)
  #allangles <- c(0, 30, 45, 60, 90)
  angle <- sample(allangles, 1)
  drawunitcircle()
  addangle(angle)
  func <- sample(c("sin", "cos", "tan"), 1)
  cat("What is", func, angle, "?\n")
  cat("Answer as a decimal with two decimal places.\n")
  cat("Hint: √3/2 = .87; √2/2 = .71; √3 = 1.73; 1/√3 = .58; enter 9999 for undefined.\n")
  solution <- dplyr::case_when(
    func == "sin" ~ sin(angle*pi/180),
    func == "cos" ~ cos(angle*pi/180),
    func == "tan" ~ tan(angle*pi/180)
  )
  solution <- round(solution, 2)
  if (solution > 1000000) solution <- 9999
  getresponse(solution)
}


pythagorean_identity <- function() {
  angle <- sample(72,1)*5
  drawunitcircle()
  addangle(angle, label = FALSE)
  quad <- quadrant(angle)
  func <- sample(c("sin", "cos"), 1)
  otherfunc <- ifelse(func == "sin", "cos", "sin")
  value <- round(ifelse(func == "sin", sin(angle*pi/180), cos(angle*pi/180)), 3)

  cat("What is", otherfunc, "A, if", func, "A = ", value, "and\n")
  cat("A is in Quadrant", quadrant(angle), "? Round to 2 decimal points.\n")
  solution <- round(ifelse(func == "sin", cos(angle*pi/180), sin(angle*pi/180)), 2)
  getresponse(solution)
}
