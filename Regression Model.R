# Set working directory
setwd("C:\\Users\\Piyush Pallav\\Google Drive\\GitHub\\Regression-Models")
library(ggplot2)
library(e1071)
library(rpart)
library(randomForest)

# Importing Dataset
dataset=read.csv('dataset.csv')
dataset1=dataset

#----------------------------------------REGRESSIONS----------------------------------------------
#Linear Regression
regressor1 = lm(formula = Price ~ ., data = dataset)

#Polynomial Regression
dataset1$Years2=dataset1$Years^2
regressor2 = lm(formula = Price ~ ., data = dataset1)

#SVR Regression
regressor3 = svm(formula = Price ~., data = dataset, type = 'eps-regression')

#Decision Tree Regression
regressor4 = rpart(formula = Price ~., data = dataset, control = rpart.control(minsplit = 10))

#Random Forest Regression
regressor5 = randomForest(x = dataset[1], y=dataset$Price, ntree = 500)

#----------------------------------------PREDICTIONS----------------------------------------------
#Predicting a new Result
y_linear = predict(regressor1, data.frame(Years = 115))
y_poly = predict(regressor2, data.frame(Years = 115,Years2 = 115^2))
y_svr = predict(regressor3, data.frame(Years = 115))
y_decision = predict(regressor4, data.frame(Years = 115))
y_randforest = predict(regressor5, data.frame(Years = 115))

#---------------------------------------VISUALISATION----------------------------------------------
#Visualizing Regression Graph {for higher resolution and smooth curve}
x_grid = seq(min(dataset$Years),max(dataset$Years),0.01)
ggplot()+
  geom_point(aes(x=dataset$Years,y=dataset$Price), color= "red1")+
  geom_line(aes(x=x_grid,y=predict(regressor1, newdata = data.frame(Years=x_grid)), color= "Linear"))+
  geom_line(aes(x=dataset1$Years ,y=predict(regressor2, newdata = dataset1), color= "Polynomial"))+
  geom_line(aes(x=x_grid,y=predict(regressor3, newdata = data.frame(Years=x_grid)), color= "SVR"))+
  geom_line(aes(x=x_grid,y=predict(regressor4, newdata = data.frame(Years=x_grid)), color= "Decision Tree"))+
  geom_line(aes(x=x_grid,y=predict(regressor5, newdata = data.frame(Years=x_grid)), color= "Random Forest"))+
  ggtitle("Vintage Collection")+
  xlab('Years')+
  ylab('Price (Rupees)')+
  scale_colour_manual(name='Regression Type', values=c('Linear'='grey69','Polynomial'='skyblue2','SVR'='seagreen3','Decision Tree'='grey', 'Random Forest'='burlywood4'))