library(ggplot2)
library(forecast)

setwd("C:/Users/SBT-Lyan-AI/Documents/dissertation/")

data <- read.csv('data/sber.csv')

CUT_UP = 0.9
train_length <- floor(length(data$Date) * CUT_UP)
data_length <- length(data$Date)
test_length <- data_length - train_length

train <- data[1:train_length,]
test <- data[train_length:data_length,]

arima_model <- arima(data$Open, order=c(2,0,2))
predicted <- predict(arima_model,1)


#------------------
#predicting

values_to_predict = 10
prediction = list(test_length)


lastSuccess = 0

for(i in 1:(test_length - values_to_predict)){
  
  lastSuccess = i
  
  tryCatch({
    arima_model <- arima(test[i:(i+values_to_predict),]$Open, order=c(1,0,1))
  }, error=function(e){})
  
  pred <- predict(arima_model,1)$pred[1]
  prediction[i] <- pred
}
prediction <- as.data.frame(as.numeric(prediction))
#-------------
#plotting
test_values <- as.data.frame(test[1:lastSuccess,]$Open)
index <- as.data.frame(1:lastSuccess)

result_data <- as.data.frame(cbind(test_values, prediction, index))
colnames(result_data) <- c("test", "pred", "index")

ggplot(data=result_data, aes(x=index, group=1)) +
  geom_line(data=result_data, aes(y=pred), color="red") + 
  geom_line(data=result_data, aes(y=test), color="green")

#-------
#holt winters

holt_model <- holt(train$Open, alpha=0.2, h=3)
holt_pred <- predict(holt_model, 3)

plot(holt_pred, type="l", col="green")
lines(data$Open, type="l", col="red")

#--------

exp_test <- fitted(ses(test$Open), alpha=0.5, h=2)
plot(exp_test, type="l", col="green")
lines(test$Open, type="l", col="red")

source("src/r/image_prediction.R")

result <- (predict_image(test$Open))
plot(test$Open, type="l", col="green")
lines(result[30,1:127], type="l", col="black")
lines(result[25,1:127], type="l", col="red")
lines(result[35,1:127], type="l", col="cyan")
lines(result[40,1:127], type="l", col="blue")



