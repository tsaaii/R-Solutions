# In preparing for class on Wed Jun 27, I think the core reading in K&S Chapter 8 is 8.1-8.11.
# 
# In K&S Chapter 7, please complete Activities 9, 10, 15, 17, 20, 21, 22, 27, 34.
# Poisson log-linear regression uses function "glm", argument "family=poisson", and an "offset" function.
# Submit using the file name "KS-Ch8.R".

data <- read.csv("C8 CancerCluster.csv", header = TRUE)

#Q9)
cancer_count <- sapply(1:10000,FUN = function(x){
  sim <- sapply(1:1138, FUN = function(x){
    resample <- sample(c(0,1), size=1, prob=c(1-0.00326*25,0.00326*25))
    })
  cancer_count <- length(sim[sim == 1])
})
hist(cancer_count, xlab = "Cancer Counts", main = "Histogram of Cancer Counts for 25 years of Exposure")
p_25 <- length(cancer_count[cancer_count >= 67])/10000
#Q10)
cancer_count <- sapply(1:10000,FUN = function(x){
  sim <- sapply(1:1138, FUN = function(x){
    resample <- sample(c(0,1), size=1, prob=c(1-0.00326*12.5,0.00326*12.5))
  })
  cancer_count <- length(sim[sim == 1])
})

hist(cancer_count, xlab = "Cancer Counts", main = "Histogram of Cancer Counts for 12.5 years of Exposure")
p_12.5 <- length(cancer_count[cancer_count >= 67])/10000

#Q15)

# a
temp <- data[which(data$Location == "BGA"),]
poisson_model_BGA <- glm(Cases~MedianAge,  family = poisson(link = "log"), offset = log(Person.years), data = temp)
coeff_BGA <- coefficients(poisson_model_BGA)

# b
diff <- temp$Rate[temp$MedianAge == 62] - 235.5
# The expected cancer incidence rate is lower than given. Since the values in the fit mode has a higher median age than given, cancer incident rate will be lower than the table.

#Q17)
temp <- data[which(data$Location == "CTR"),]
poisson_model_CTR <- glm(Cases~MedianAge,  family = poisson(link = "log"), offset = log(Person.years), data = temp)
coeff_CTR <- coefficients(poss.model.CTR)
# The beta_0 value is lower for CTR but beta_1 is higher for CTR. The CTR locations are increasing.

#Q20)
poisson_model <- glm(Cases~MedianAge,  family = poisson(link = "log"), offset = log(Person.years), data = data)
coeff <- coefficients(poisson_model)
# These are closer to the CTR locations.

#Q21)
temp <- data
temp$Location <-as.character(temp$Location)
temp$Location[temp$Location == "CTR"] <- 1
temp$Location[temp$Location == "BGA"] <- 0
temp$Location <-as.numeric(temp$Location)

poisson_model_location <- glm(Cases~Location,  family = poisson(link = "log"), offset = log(Person.years), data = temp)
coeff_location <- coefficients(poisson_model_location)
CTR.higher <- exp(coff.location[2])
# Rate of cancer at CTR location is 2.91 times higer than the one at BGA.

#Q22)
poisson_model_twocov <- glm(Cases~Location+MedianAge,  family = poisson(link = "log"), offset = log(Person.years), data = temp)
coeff_towcov <- coefficients(poisson_model_twocov)

#Q27)
poisson_model_twocov_interac <- glm(Cases~MedianAge*Location,  family = poisson(link = "log"), offset = log(Person.years), data = temp)
coff.towcov.interac <- coefficients(poisson_model_twocov_interac)

#Q34)
# a
waldtest<- summary(poisson_model_twocov_interac)$coefficients[4,1]/ summary(poisson_model_twocov_interac)$coefficients[4,2]
dev.res <- residuals(poisson_model_twocov_interac)
dev.stat <- sum(dev.res^2)
# walds statstic is 2.5 is proportional to a  higher p-value. So we can't reject the null hypothesis.

# b
g.LRT <- anova(glm(Cases~MedianAge*Location+I(MedianAge^2),  family = poisson(link = "log"), offset = log(Person.years), data = temp), test="LRT")

print(temp$Rate[temp$MedianAge == 62])
