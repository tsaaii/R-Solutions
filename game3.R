#Q19

game <- read.csv("C2 Games1.csv")
game$Type <- as.numeric(game$Type)
game$Type[game$Type == 2] <- 0
y<- mean(game$Time)
y1<- mean(game$Time[game$Type == 0])
y2<- mean(game$Time[game$Type == 1])

#Q20
effect.size.of.standard <- (y1 - y)
effect.size.of.color <- (y2 - y)

#Q21

plot(c(y2,y1)~c(0,1), type="o", xaxt = "n", xlab= "type", ylab= "mean time", main="main effects plot" )
axis(1,at=0:1, labels=c("Color","Standard"))

#Q23
lmodel <- lm(game$Time~game$Type)
An_of_var <- anova(lmodel)

f <- An_of_var$`F value`[1]
p <- An_of_var$`Pr(>F)`[1]

#P value concludes that

#26

residual <-resid(lmodel)
hist(residual,xlab = "residual", ylab = "frequency")
stripchart(game$Type~residual, xlab="type", ylab="residual", main="type vs residual plot", pch= 20)
stripchart(game$studentID~residual,xlab="order", ylab="residual", main="order vs residual plot", pch= 20 )

#yes, the residuals are appx normal
#order vs residual is not an iid
#type vs residual is iid
