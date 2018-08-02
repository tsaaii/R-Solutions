game <- read.csv("C2 Games1.csv")

#Q4


plot(Time~Type, data= game , main =" graph showing time vs type")
mean <- aggregate(game$Time, by=list(game$Type), FUN = mean)
sd<- aggregate(game$Time, by=list(game$Type), FUN = sd)
#the mean of both the types is almost the same and the standard deviation is almost equal
#there is nothing unusal here. and there are no outliers too.

#Q7
games_linear <- lm(Time~Type, data = game)
games_residual <- resid(games_linear)
hist(games_residual, xlab="residual", main = "histogram of residuals")

#No the distribution is not normal

#Q10

game$Type <- as.numeric(game$Type)
game$Type[game$Type == 2] <- 0
tt <- t.test(game$Time[game$Type == 0],game$Time[game$Type == 1], var.equal = TRUE)



