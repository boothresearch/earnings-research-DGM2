earnings <- read.csv("https://raw.githubusercontent.com/avehtari/ROS-Examples/master/Earnings/data/earnings.csv")

# File to write output of lm
write2file <- function(fname, regformula, regdata) {
  fitted.model <- lm(formula = regformula, data = regdata)
  sink(file = fname, append = FALSE) # create an empty file named fname
  print(Sys.time(), quote = FALSE)
  print(summary(fitted.model))
  closeAllConnections()
  fitted.model
}

# [1] Regress earnings on height
reg01 <- function() {
  # fit a linear model to the data
  fitted.model <- write2file("reg01.txt", earn ~ height, earnings)
  
  # graph the data and the regression line
  png("reg01.png") # open a png file
  plot(earnings$height, earnings$earn, xlim = c(40, 90), xlab = "height", ylab = "earnings")
  abline(fitted.model, col = "red") # plot the regression line
  dev.off() # close the file
}

# [2] Regress earnings on height controlling for male
reg02 <- function() {
  # fit a linear model to the data
  fitted.model <- write2file("reg02.txt", earn ~ height + male, earnings)
  
  # graph the data and the regression line
  png("reg02.png") # open a png file
  plot(earnings$height, earnings$earn, xlim = c(40, 90), xlab = "height", ylab = "earnings")
  abline(fitted.model, col = "red") # plot the regression line
  dev.off() # close the file
}

# [3] Regress earnings on height controlling for male and it's interaction with height
reg03 <- function() {
  # fit a linear model to the data
  fitted.model <- write2file("reg03.txt", earn ~ height + male + height:male, earnings)
  
  # graph the data and the regression line
  png("reg03.png") # open a png file
  plot(earnings$height, earnings$earn, xlim = c(40, 90), xlab = "height", ylab = "earnings")
  abline(fitted.model, col = "red") # plot the regression line
  dev.off() # close the file
}

reg01()
reg02()
reg03()