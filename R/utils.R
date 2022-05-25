#' @importFrom graphics abline text arrows plot.new
#' @importFrom utils read.csv

quadrant <- function(angle) {
  quadnames <- c("first", "second", "third", "fourth")
  angle <- angle %% 360
  quad <- floor(angle/90+1)
  names(quad) <- quadnames[quad]
  quad
}

ref_angle <- function(angle) {
  quad <- quadrant(angle)
  angle <- angle %% 360
  switch(quad, angle, 180-angle, angle-180, 360-angle)
}

get_other_angle <- function(func, angle) {
  allfunc <- c("sin", "cos", "tan")
  switch(which(allfunc == func),
         switch(quadrant(angle), 180-angle, 180-angle, 540-angle, 540-angle),
         360 - angle,
         switch(quadrant(angle), 180+angle, 180+angle, angle-180, angle-180))
}

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

compliment <- function() {
  phrases <- c("Way to go!", "You did it!", "Math is you!", "Go Victor!", "You're a pro!", paste(rep(emo::ji("clap"), 3), collapse = ""), "Woohoo!", "You rock.", "You're the Federer of math.")
  cat(sample(phrases, 1), "\n")
  }

encouragement <- function() {
  phrases <- c("You can do this!", "Try another problem.", "Keep at it.", "Don't give up.", "Practice makes progress.", "The more you practice the better you'll get.")
cat(sample(phrases, 1), "\n")
}


getresponse <- function(solution) {
  if (length(solution) > 1 && solution[1]==solution[2]) solution <- solution[1]
  ans <- readline()
  if (ans == "s") return()
  if (is.na(ans) || suppressWarnings(is.na(as.numeric(ans)))) {
    alldone()
    return(-1)
    } else {
    if(as.numeric(ans) %in% solution) {
      usethis::ui_done("Correct!")
      compliment()
      } else {
      if (length(solution) == 1) {
        usethis::ui_oops(paste("Incorrect. The answer is:", solution))
        encouragement()
      } else {
        usethis::ui_oops("Incorrect. Try again.")
      }
      }
    if (length(solution) > 1) {
      cat("Please enter a second answer.")
      ans2 <- readline()
      if((ans == ans2) && (ans %in% solution)) {
        cat("You already entered that solution.\n")
        return()
      }
      if(as.numeric(ans2) %in% solution) {
        usethis::ui_done("Correct!")
        compliment()
      } else {
        usethis::ui_oops(paste("Incorrect. The answers are:", solution[1], "and", solution[2], "."))
        encouragement()
      }
    }
    }
}

alldone <- function() {
  cat("Good-bye Zupie! Hope to see you soon.\n")
}
