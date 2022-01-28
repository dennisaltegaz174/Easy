data = read.csv("C://Users//adm//Downloads//bmd.csv")
# Descriptive statistics
library("psych")
describeBy(data)

# Question  2
#Logistic regression
#converting fracture to 0 and  1
data$fracture <-ifelse(data$fracture == "fracture",1,0)
data$medication<-ifelse(data$medication=="No medication",0,1)
data$sex<-ifelse(data$sex=="F",0,1)


glm <-glm(fracture~age+sex+(bmi=(weight_kg/height_cm^2))+medication+waiting_time+bmd,
          data=data,family = binomial)
#a)
summary(glm)
# There is some  relationship between outcome and some variables.

#b) Significance of  each  predictor
# bmi,medication and waiting time  are all insignificant as their p-values are greater then 0.05
#bmd was a significant variable as is p-  value was less than 0.05. Sex was slightly significant in the model

#c) variables to consider in a multiple  logistic regression
# i) bmd
# ii) sex
# iii) medication


# Question 3
glm_2 <- glm(fracture~age+sex+(bmi=(weight_kg/height_cm^2))+medication+waiting_time+bmd,
             data=data,family = binomial)
# a)Correlation between variables age,sex,bmi,medication ,waiting_time and bmd
data_1 =data
data_1$bmi<-(data_1$weight_kg/data_1$height_cm^2)
cor(data_1[, c("age","sex","bmi","medication" ,"waiting_time","bmd")])

# b) Overall fit of the model


# c)

#d) Variables significantly associated with fracture
summary(glm_2)
 # bmd
#medication
#sex

#e) Multicoloniearity
car::vif(glm_2)
