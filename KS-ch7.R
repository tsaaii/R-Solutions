# In preparing for class on Wed Jun 20, I think the core reading in K&S Chapter 7 is 7.1-7.9.
# The concepts in logic regression will be covered in the Rossi chapter for next week.
# 
# In K&S Chapter 7, please complete Activities 3, 5, 6, 7, 8, 9, 11, 12, 14.
# Logistic regression uses function "glm" and the argument "family=binomial".
# Submit using the file name "KS-Ch7.R".

#Q3)

data<-read.csv('C7 Shuttle.csv')
plot(data$Success~data$Temperature)
fit<-lm(data$Success~data$Temperature)
abline(fit)
model <- lm( Success ~ Temperature, data=data)
new.data <-data.frame(Temperature=c(60,85))
predict(model,new.data)

# For a temperature of 60F it is 0.338, for 85 it is 1.272

#Q5)
beta0 <- c(-10, -5, 10, 5)
beta1 <- c(0.5, 1, 1.5,-0.5, -1, -1.5)
pis <- sapply(1:3, FUN = function(x){
  pi <- exp(beta0[1]+beta1[x]*data$Temp)/(1+exp(beta0[1]+beta1[x]*data$Temp))
})
plot(data$Temp,pis[,1],pch=19, main ="Beta0 = -10")
points(data$Temp,pis[,2], col = "red")
points(data$Temp,pis[,3], col = "blue", pch= 20)
pis <- sapply(1:3, FUN = function(x){
  pi <- exp(beta0[2]+beta1[x]*data$Temp)/(1+exp(beta0[2]+beta1[x]*data$Temp))
})

plot(data$Temp,pis[,1],pch=20, main = "beta0 = -5")
points(data$Temp,pis[,2], col = "red")
points(data$Temp,pis[,3], col = "blue", pch= 20)
pis <- sapply(1:3, FUN = function(x){
  pi <- exp(beta0[3]+beta1[x+3]*data$Temp)/(1+exp(beta0[3]+beta1[x+3]*data$Temp))
})

plot(data$Temp,pis[,1],pch=20, main = "beta0 = 5")
points(data$Temp,pis[,2], col = "red")
points(data$Temp,pis[,3], col = "blue", pch= 20)


pis <- sapply(1:3, FUN = function(x){
  pi <- exp(beta0[4]+beta1[x+3]*data$Temp)/(1+exp(beta0[4]+beta1[x+3]*data$Temp))
})

plot(data$Temp,pis[,1],pch=20, main = "beta0 = 10")
points(data$Temp,pis[,2], col = "red")
points(data$Temp,pis[,3], col = "blue", pch= 20)

# a
# The plots look similar to eachother. As expected Beta 0 shifts the entire prediction while beta 1 changes the slope of the prediction.
# b
# The value of 0.99 has the steepest slope.

#Q6)
ls_mod <- lm(Success~Temperature, data = data)
log_mod <- glm(Success~Temperature, data = data, family = binomial)
beta.ls <- ls_mod$coefficients
beta.log <-  log_mod$coefficients
# The intercept for the log model is significantly lower than the least square model. 
# The slope for the log model is much higher than the least square model.

#Q7
test <- data.frame(Temperature= c(31, 50, 75))
prediction <- predict(log_mod,test,type="response")
print(prediction)

#Q8
test <- data.frame(Temperature= c(60, 70))
logodds <- predict(log_mod,test)
print(logodds)

#Q9
#If x is increased by 10 exp(beta 1) will be increased by 10 times which means that the odds would be increased by exp(10*beta1) times.

#Q11

plot(Success~Temperature, data = data, main="Success vs Temperature", pch =19,type='n')
abline(ls_mod$coefficients[1],ls_mod$coefficients[2])
plot(Success~Temperature, data = data, main="Success vs Temperature (log)", pch =19,type='n')
curve(predict(log_mod,data.frame(Temperature=x),type="response"),add=TRUE)

#Q12
odds.ratio <- exp((60-31)*log_mod$coefficients[2])

# 14
p_glm <- coef(summary(log_mod))[,4]
logtest <- anova(log_mod, test='LRT')
p_lrt <- logtest$`Pr(>Chi)`[2]
# The values with greater and less than z = p value / 2 for both intercept and slope would be rejected as erroneous data.









