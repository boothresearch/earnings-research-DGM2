earnings <- read.csv("https://raw.githubusercontent.com/avehtari/ROS-Examples/master/Earnings/data/earnings.csv")

fitted.model <- lm(earnings$earn ~ earnings$height)
print(summary(fitted.model))