library(Hmisc)
data<- read.csv("C://Users//adm//Documents//Datasets//covid_19 prj easy//COVID19_line_list_data.csv")
describe(data) 
alternatively
library(psych)
describe.by(data)

# Cleaned up death column
unique(data$death)
# Cleaning the death column
data$death_dummy <-as.integer(data$death !=0)
unique(data$death_dummy)
# Death rate
sum(data$death_dummy)/ nrow(data)
# we've an average death rate of 0.05806452

# claim: The average person who dies from covid 19 is older than those that survive
# Age
dead = subset(data,death_dummy == 1)
alive = subset(data,death_dummy == 0)
mean(dead$age, na.rm = TRUE)
mean(alive$age,na.rm = TRUE)
# is this statistically significant?
t.test(alive$age,dead$age,alternative = "two.sided",conf.level = 0.95)

# If P-value is <0.05 we reject null hypothesis
# here the  p-value is equal to zero so we reject the null hypothesis and 
#conclude that this is statistically significant.


# GENDER
# Claim:gender has no effect on death due to covid
men = subset(data, gender=="male")
women = subset(data,gender == 'female')
mean(men$death_dummy, na.rm = TRUE) # 8.4%
mean(women$death_dummy,na.rm = TRUE)#3.7%
# Is this statistially significant?
t.test(men$death_dummy,women$death_dummy, alternative = "two.sided",conf.level = 0.99)
# we are 99% confident that men have from 0.8% to  8.8% higher chance of dying
# Since the p-value is < 0.05 we reject the null hypothesis
# Conclusion there is statistical sigificance(That men have a higher death rate than women)