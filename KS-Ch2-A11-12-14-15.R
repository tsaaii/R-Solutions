#Q11
game <- read.csv("C2 Games1.csv")
game$Type <- as.numeric(game$Type)
game$Type[game$Type == 2] <- 0


game_regg <- lm(Time ~ Type, # regression formula
              data=game) # data set
# Summarize and print the results
summary(game_regg)

#Q12
tt1 <-t.test(game$Time[game$Type == 0],game$Time[game$Type == 1])
# mean1-mean2 is greater than 0 hence beta1 is also greater than 0

#Q14.

res_game <-resid(game_regg) 
hist(res_game, xlab="residuals", ylab = "Frequency")
plot(res_game~game$studentID, type ="o", xlab = "order", ylab = "residual", main ="order vs residual plot")
# graphs looks the same because, residual is nothing but the difference from the mean value which will be again the same but just scaled by a factor

#Q15
plot(game$Time~game$Type, xlab="type", ylab="time", pch = 20, main="graph showing time vs type")
abline(game_regg)
