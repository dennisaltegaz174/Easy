# Load the dataset
library(dplyr)
library(caret)
data("Boston",package = "MASS")
Boston = as.data.frame(Boston)
str(Boston)

# Splitting the data into training and testing data
set.seed(123)
training.samples <- Boston$medv %>% createDataPartition(p = 0.8,list=FALSE)
train.data <- Boston[training.samples,]
test.data <-Boston[-training.samples]  

#Scatter Plot
ggplot(train.data,aes(lstat, medv))+
  geom_point(aes(color=lstat))+
  stat_smooth()
# Computing the linear regression model
#Model building
model<- lm(medv ~ lstat,data = train.data)
# Making predictions
as.data.frame(test.data)
predictions <- model %>% predict(test.data)
pred_lm <- predict(model,test.data)

# Model Performance
data.frame(RMSE = RMSE(predictions,test.data$medv),R2 = R2(predictions,test.data$medv))
# Visualize the data
ggplot(train.data,aes(lstat,medv))+
  geom_point()+
  stat_smooth(method = lm,formula = y~x)


# Polynomial Regression
lm(medv ~ lstat+I(lstat^2),data=train.data)
# alternatively
lm(medv~poly(lstat, 2 ,raw = TRUE),data = train.data)

# polynomial(6th order polynomial)
lm(medv~poly(lstat, 6, raw=TRUE),data = train.data)%>%summary()
# From the output it can be seen that polynomial terms beyond the fifth order are not significant
# so creating model up to fifth polynomial
 model1 <- lm(medv~poly(lstat,5,raw = TRUE),data = train.data)
#Making predictions
 pred_2 = predict(model,test.data)
# Model Performance 
 data.frame(RMSE = RMSE(predictions,test.data$medv),R2 = R2(predictions,test.data$medv))
 
 
 # Visualizing the fifth polynomial regression
 ggplot(train.data,aes(lstat,medv))+
   geom_point()+
   stast_smooth(method = lm ,formula = y~poly(x,5,raw = TRUE))
 
# Deciding on degree
# One way is by using hypothesis testing 
# This can be done using  ANOVA
fit_1 = lm(medv~lstat,data = train.data)
fit_2 = lm(medv~poly(lstat,2),data = train.data)
fit_3 = lm(medv~poly(lstat,3),data = train.data)
fit_4 = lm(medv~poly(lstat,4),data = train.data)
fit_5 = lm(medv~poly(lstat,5),data = train.data)
fit_6 = lm(medv~poly(lstat,6),data = train.data)
print(anova(fit_1,fit_2,fit_3,fit_4,fit_5,fit_6))
