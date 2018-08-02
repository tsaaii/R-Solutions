#jul 11

# In preparing for class on Wed Jul 11, I think the core reading in K&S Chapter 5 is 5.1 to 5.9.
# 
# In K&S Chapter 5, please complete Activities 1, 2, 8, 15.
# Submit using the file name "KS-Ch5.R".
# 
# I observe the following:
#   Function "aov" form the analysis-of-variance model (same as a regression model).
# Function "factor" will convert a numeric variable (such as "Student" here) to a factor.
# A model term of the form "A : B" is the AB interaction term.
# A model term of the form "A * B" is equivalent to "A + B + A : B", i.e., all terms and interactions.
# An "Error( )" term in the model will do stratification of error.
# The URL "https://www.statmethods.net/stats/anova.html" explains a bit about model formulae for analysis of variance.
# Function "anova" will collect the results into an analysis-of-variance table.

#Q1)
data <- read.csv('C5 Memory.csv')
# a
# The first hypothesis to be tested is the mean test score of concrete wordlist which is equal to mean test score of abstract wordlist.
# h_o: mu_c = mu_a vs. h_a: mu_c != mu_a

# The second hypothesis to be tested is the mean test score of distracter math and poetry.
# h_o: mu_p - mu_m vs h_a: mu_c != mu_a

# The last hypothesis to be tested is the effect of wordlist on distracter and vice-versa.
# h_o: Effect of wordlist for both distracters is same. Effect of distracters is same for both wordlist. 
# h_a: distracter and wordlist interact with eachother.

# b
anva <- aov(Score~Wordlist*Distracter, data = data)
print(summary(anva))

# c
plot(Score~Wordlist, data = data, main = "Score vs Wordlist",title='normal probability plot')
plot(Score~Distracter, data = data, main = "Score vs Distracter", title='individual value plot of score')
qqnorm(anva$residuals)
qqline(anva$residuals)
#box plot shows that outliers and qqplot doesnt have a normal distribution. So, the equal variance assumption is violated.

# d
# The p-value = 0.015 corresponding to the hypothesis about word list, thus at the alpha level of 0.05 we can reject the null hypothesis 
# However the p-values for Distracter and Distracter:Wordlist is high and the null hypothesis cannot be rejected.
plot(c(6,10)~c(0,6), type='n', xaxt = 'n', xlab="Factors", ylab = "Score", main="Main Effects Plot")

points(c(0, 2, 4, 6),c(mean(subset(data, data$Wordlist=="abstract")$Score),mean(subset(data, data$Wordlist=="concrete")$Score), mean(subset(data, data$Distracter=="math")$Score),mean(subset(data, data$Distracter=="poetry")$Score)), pch = 19)

lines(c(0,2),c(mean(subset(data, data$Wordlist=="abstract")$Score),mean(subset(data, data$Wordlist=="concrete")$Score)))
lines(c(4,6),c(mean(subset(data, data$Distracter=="math")$Score),mean(subset(data, data$Distracter=="poetry")$Score)))
abline(h=mean(data$Score))
axis(1, at=c(0, 2, 4, 6), labels=c("abstract", "concrete", "math", "poetry"))
text(3,7.8, "Mean")

interaction.plot(data$Distracter,data$Wordlist,data$Score,type = "o", leg.bty="o",  pch = 19, ylab = "Mean of Score", xlab="Distracter", main = "Interaction plot", trace.label = "Wordlist")

#Q2)

anva1 <- aov(Score~factor(Student)+Wordlist+Distracter+Wordlist:Distracter, data = data)
print(summary(anva1))
# a
# # The first hypothesis to be tested is the mean test score of concrete wordlist which is equal to mean test score of abstract wordlist.
# h_o: mu_c = mu_a vs. h_a: mu_c != mu_a
# Mean score for each for the wordlist parameters is compared. P-value 0.0045. The null hypothesis can be rejected.
# Mean score for each of the distracter parameters is compared. P-value 0.238. The null hypothesis cannot be rejected.
# Effect of distacter and wordlist on eachother. P-value 0.647. The null hypothesis cannot be rejected.
# b
# The p-value for student is slighty high at 0.296. means students do have an effect on the variability of the data.
# c
plot(c(6,10)~c(0,2), type='n', xaxt = 'n', xlab="Factors", ylab = "Score", main="Main Effects Plot (WordList)")
points(c(0, 2),c(mean(subset(data, data$Wordlist=="abstract")$Score),mean(subset(data, data$Wordlist=="concrete")$Score)), pch = 19)
lines(c(0,2),c(mean(subset(data, data$Wordlist=="abstract")$Score),mean(subset(data, data$Wordlist=="concrete")$Score)))
abline(h=mean(data$Score))
axis(1, at=c(0, 2), labels=c("abstract", "concrete"))
text(0.5,mean(data$Score), "Mean")
plot(c(6,10)~c(0,2), type='n', xaxt = 'n', xlab="Factors", ylab = "Score", main="Main Effects Plot (Distracter)")
points(c(0, 2),c(mean(subset(data, data$Distracter=="math")$Score),mean(subset(data, data$Distracter=="poetry")$Score)), pch = 19)
lines(c(0,2),c(mean(subset(data, data$Distracter=="math")$Score),mean(subset(data, data$Distracter=="poetry")$Score)))
abline(h=mean(data$Score))
axis(1, at=c(0, 2), labels=c("math", "poetry"))
text(0.5,mean(data$Score), "Mean")
plot(c(5,12)~c(0,12), type='n', xlab="Factors", ylab = "Score", main="Main Effects Plot (Student)")
points(c(1,2, 3, 4, 5, 6, 7, 8, 9, 10, 11,12),c(mean(subset(data, data$Student=="1")$Score),mean(subset(data, data$Student=="2")$Score),mean(subset(data, data$Student=="3")$Score), mean(subset(data, data$Student=="4")$Score),mean(subset(data, data$Student=="5")$Score),mean(subset(data, data$Student=="6")$Score),mean(subset(data, data$Student=="7")$Score),mean(subset(data, data$Student=="8")$Score),mean(subset(data, data$Student=="9")$Score),mean(subset(data, data$Student=="10")$Score),mean(subset(data, data$Student=="11")$Score),mean(subset(data, data$Student=="12")$Score)), pch = 19)
lines(c(1,2, 3, 4, 5, 6, 7, 8, 9, 10, 11,12),c(mean(subset(data, data$Student=="1")$Score), mean(subset(data, data$Student=="2")$Score),mean(subset(data, data$Student=="3")$Score),mean(subset(data, data$Student=="4")$Score),mean(subset(data, data$Student=="5")$Score),mean(subset(data, data$Student=="6")$Score),mean(subset(data, data$Student=="7")$Score),mean(subset(data, data$Student=="8")$Score),mean(subset(data, data$Student=="9")$Score),mean(subset(data, data$Student=="10")$Score),mean(subset(data, data$Student=="11")$Score),mean(subset(data, data$Student=="12")$Score)))
abline(h=mean(data$Score))
text(5.8,mean(data$Score), "Mean")
# p-value is small.While the p-value for distracter is higher which corresponding with the main effects plot as the mean for distracter has or is closer to the mean of the total score. 
# d
hist(anva1$residuals)
qqnorm(anva1$residuals)
qqline(anva1$residuals)
# The normal probability plot and the histogram of the residuals suggest that the residuals do not follow normal distribution
#Q8)
# h_o: means for majors are equal. mu_cs = mu_math = mu_hist = my_eng, h_a: means are differrent.
anva2 <- anva(Score~Major+Wordlist*Distracter+Error(Major/factor(Student)), data = data)
print(summary(anva2))
p <- 1- pf(3.188/8.208,3, 8)
hist(anva2$Within$res)
qqnorm(anva2$Within$residuals)
qqline(anva2$Within$residuals)

# main effects plot major
plot(c(7,9)~c(0,3), type='n', xaxt = 'n', xlab="Factors", ylab = "Score", main="Main Effects Plot(Major)")
points(c(0, 1, 2, 3),c(mean(subset(data, data$Major=="CS")$Score), mean(subset(data, data$Major=="Eng")$Score),mean(subset(data, data$Major=="Hist")$Score),mean(subset(data, data$Major=="Math")$Score)), pch = 19)
lines(c(0,1,2,3),c(mean(subset(data, data$Major=="CS")$Score),mean(subset(data, data$Major=="Eng")$Score),mean(subset(data, data$Major=="Hist")$Score),mean(subset(data, data$Major=="Math")$Score)))
abline(h=mean(data$Score))
axis(1, at=c(0, 1, 2, 3), labels=c("CS", "Eng", "Hist", "Math"))
text(2,mean(data$Score), "Mean")

# main effects plot wordlist
plot(c(6,10)~c(0,1), type='n', xaxt = 'n', xlab="Factors", ylab = "Score", main="Main Effects Plot(Wordlist)")
points(c(0, 1),c(mean(subset(data, data$Wordlist=="abstract")$Score), mean(subset(data, data$Wordlist=="concrete")$Score)), pch = 19)
lines(c(0,1),c(mean(subset(data, data$Wordlist=="abstract")$Score),mean(subset(data, data$Wordlist=="concrete")$Score)))

abline(h=mean(data$Score))
axis(1, at=c(0, 1), labels=c("abstract", "concrete"))
text(0.1,mean(data$Score), "Mean")

# main effects plot distracter
plot(c(6,10)~c(0,1), type='n', xaxt = 'n', xlab="Factors", ylab = "Score", main="Main Effects Plot(Distracter)")
points(c(0, 1),c(mean(subset(data, data$Distracter=="math")$Score),mean(subset(data, data$Distracter=="poetry")$Score)), pch = 19)
lines(c(0,1),c(mean(subset(data, data$Distracter=="math")$Score),mean(subset(data, data$Distracter=="poetry")$Score)))
abline(h=mean(data$Score))
axis(1, at=c(0, 1), labels=c("math", "poetry"))
text(0.1,mean(data$Score), "Mean")

# With the p-values.
# For Major: Not reject null hypothesis,Wordlist: Reject null hypothesis, For Distracter: Not reject null hypothesis, For Wordlist:Distracter: Not reject null hypothesis, From the qqplot and the histogram, the residuals follow normal distribution.

#Q15
anva3 <- aov(Score~Major+Major:Wordlist+Major:Distracter+Wordlist*Distracter+Error(Major/factor(Student)), data = data)
print(summary(anva3))
interaction.plot(data$Wordlist,data$Major,data$Score,type = "o", leg.bty="o",  col= 1:4, fixed=TRUE, legend = TRUE, pch = 19, ylab = "Mean of Score", xlab="Distracter", main = "Interaction plot", trace.label = "Major")
interaction.plot(data$Distracter,data$Major,data$Score,type = "o", leg.bty="o",  col= 1:4, fixed=TRUE, legend = TRUE, pch = 19, ylab = "Mean of Score", xlab="Distracter", main = "Interaction plot", trace.label = "Major")
interaction.plot(data$Distracter,data$Wordlist,data$Score,type = "o", leg.bty="o",  col= 1:4, fixed=TRUE, legend = TRUE, pch = 19, ylab = "Mean of Score", xlab="Distracter", main = "Interaction plot", trace.label = "Major")
