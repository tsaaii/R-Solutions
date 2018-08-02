#jul 4  

# In preparing for class on Wed Jul 4, I think the core reading in K&S Chapter 4 is 4.1 to 4.5.
# 
# In K&S Chapter 4, please complete Activities 5, 13, 15, 16, 17, 18.
# Submit using the file name "KS-Ch4.R".


data <- read.csv("C4 Popcorn.csv", header = TRUE)

#Q5)
temp_105<- subset(data, Time == '105 s' & Brand == 'Fastco')
fastco_105_mean <- mean(temp_105$PopRate)
fastco_105_median <- median(temp_105$PopRate)
fastco_105_sd <- sd(temp_105$PopRate)
fastco_105_range <- range(temp_105$PopRate)

temp_135<- subset(data, Time == '135 s' & Brand == 'Fastco')
fastco_135_mean <- mean(temp_135$PopRate)
fastco_135_median <- median(temp_135$PopRate)
fastco_135_sd <- sd(temp_135$PopRate)
fastco_135_range <- range(temp_135$PopRate)

fastco.mean.diff <- abs(fastco_105_mean - fastco_135_mean)

temp_pops<- subset(data, Time == '105 s' & Brand == 'Pop Secret')
pop_105_mean <- mean(temp_pops$PopRate)
pop_105_median <- median(temp_pops$PopRate)
pop_105_sd <- sd(temp_pops$PopRate)
pop_105_range <- range(temp_pops$PopRate)

temp_pops_135<- subset(data, Time == '135 s' & Brand == 'Pop Secret')

pop_135_mean <- mean(temp_pops_135$PopRate)
pop_135_median <- median(temp_pops_135$PopRate)
pop_135_sd <- sd(temp_pops_135$PopRate)
pop_135_range <- range(temp_pops_135$PopRate)

popsecret.mean.diff <- abs(pop_105_mean-pop_135_mean)

#Q13)
results <- summary(lm(PopRate~Brand, data = data))
f.stat.brand <- results$fstatistic[1]
p.value.brand <- pf(results$fstatistic[1], results$fstatistic[2], results$fstatistic[3],lower.tail = FALSE)
results <- summary(lm(PopRate~Time, data = data))
f.stat.Time <- results$fstatistic[1]
p.value.Time <- pf(results$fstatistic[1], results$fstatistic[2], results$fstatistic[3],lower.tail = FALSE)
results <- summary(lm(PopRate~Brand:Time, data = data))
f.stat.BrandTime <- results$fstatistic[1]
p.value.BrandTime <- pf(results$fstatistic[1], results$fstatistic[2], results$fstatistic[3],
                        lower.tail = FALSE)

# the p value for brand is 0.756 which is high so the null hypothesis can't be rejected. p value for time is 0.0153 so the null hypothesis can be rejected.p value for brandtime is 0.0177 so the null hypothesis can be rejected.

#Q15)
# a
# there is no outliers seen from the plot.

# b
sd.ratio <- fastco_135_sd/fastco_105_sd
# Equal variance assumption is valid as the figure shows similar distibution.

# c
model <- lm(PopRate~Brand:Time, data = data)
hist(model$residuals)
qqnorm(model$residuals)
qqline(model$residuals)

# graphs shows a normal distribution. 

#Q16)
#Q17)
sd_IV <- numeric()
#For Fastco brand 

temp_lounge<- subset(data, Brand == 'Fastco'&Microwave=='Lounge'&Time=='105 s')
plot(temp_lounge$PopRate~as.numeric(temp_lounge$Time), type="p", xaxt='n', main= "IVP for Fastco Lounge 105 s", pch =19, ylab = "PopRate", xlab="Time")
axis(1, at=1, labels='105 s')

sd_IV <- c(1, sd(temp_lounge$PopRate))

temp_lounge_135 <- subset(data, Brand == 'Fastco'&Microwave=='Lounge'&Time=='135 s')
plot(temp_lounge_135$PopRate~as.numeric(temp_lounge_135$Time), type="p", xaxt='n', main= "IVP for Fastco Lounge 135 s", pch =19, ylab = "PopRate", xlab="Time")
axis(1, at=2, labels='135 s')
sd_IV <- c(2, sd(temp_lounge_135$PopRate))

temp_fastco_room <- subset(data, Brand == 'Fastco'&Microwave=='Room'&Time=='105 s')
plot(temp_fastco_room$PopRate~as.numeric(temp_fastco_room$Time), type="p", xaxt='n', main= "IVP for Fastco Room 105 s", pch =19, ylab = "PopRate", xlab="Time")
axis(1, at=1, labels='105 s')
sd_IV <- c(3, sd(temp_fastco_room$PopRate))

temp_fastco_135 <- subset(data, Brand == 'Fastco'&Microwave=='Room'&Time=='135 s')
plot(temp_fastco_135$PopRate~as.numeric(temp_fastco_135 $Time), type="p", xaxt='n', main= "IVP for Fastco Room 135 s", pch =19, ylab = "PopRate", xlab="Time")
axis(1, at=2, labels='135 s')
sd_IV <- c(4, sd(temp_fastco_135$PopRate))

# Pop Secret
temp_pops_105 <- subset(data, Brand == 'Pop Secret'&Microwave=='Lounge'&Time=='105 s')
plot(temp_pops_105$PopRate~as.numeric(temp_pops_105$Time), type="p", xaxt='n', main= "IVP for Pop Secret Lounge 105 s", pch =19, ylab = "PopRate", xlab="Time")
axis(1, at=1, labels='105 s')
sd_IV <- c(5, sd(temp_pops_105$PopRate))

temp_pops_135 <- subset(data, Brand == 'Pop Secret'&Microwave=='Lounge'&Time=='135 s')
plot(temp_pops_135$PopRate~as.numeric(temp_pops_135$Time), type="p", xaxt='n', main= "IVP for Pop Secret Lounge 135 s", pch =19, ylab = "PopRate", xlab="Time")
axis(1, at=2, labels='135 s')
sd_IV <- c(6, sd(temp_pops_135$PopRate))

temp_room_105<- subset(data, Brand == 'Pop Secret'&Microwave=='Room'&Time=='105 s')
plot(temp_room_105$PopRate~as.numeric(temp_room_105$Time), type="p", xaxt='n', main= "IVP for Pop Secret Room 105 s", pch =19, ylab = "PopRate", xlab="Time")
axis(1, at=1, labels='105 s')
sd_IV <- c(7, sd(temp_room_105$PopRate))

temp_room_135 <- subset(data, Brand == 'Pop Secret'&Microwave=='Room'&Time=='135 s')
plot(temp_room_135 $PopRate~as.numeric(temp_room_135 $Time), type="p", xaxt='n', main= "IVP for Pop Secret Room 135 s", pch =19, ylab = "PopRate", xlab="Time")
axis(1, at=2, labels='135 s')
sd_IV <- c(8, sd(temp_room_135 $PopRate))

# a Everything is correlated with popping time.

# b They look similar except the pop secret in the lounge with 105 s for time where there are two pop time at 72.

# c There are no outliers.

#Q17)

sd_diff <- max(sd_IV) / min(sd_IV)

# since the ratio is 1.1, we can assume it to be having equal variance.

#Q18)

three.way.anova <- aov(PopRate~Time*Microwave*Brand, data = data)

# b
hist(three.way.anova$residuals, xlab = "Residuals", main = "Histogram of the Residuals")
qqnorm(three.way.anova$residuals)
# The histogram shows that the distribution is not normally distributed.

# c
# null hypothesis that the means of the different brands cannot be rejected as the p-value is 0.7308.
# null hypothesis that the mean of the different time can be rejected as the p-value of 0.0162.
# null hypothesis that the means of the different microwaves cannot be rejected as the p-value is 0.4426.
# Microwave does not have influence on brand to affect the poprate and also the time. The p-value is 0.5074 and 0.6357 respectively.

# d
# Random sampling and allocation could change the variance for the different factors. This would potentially invalidate the assumption of equal variances.




