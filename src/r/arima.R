library(ggplot2)
library(forecast)

setwd("C:/Users/SBT-Lyan-AI/Documents/dissertation/data/")

data <- read.csv('sber.csv')

CUT_UP = 0.7
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

holt_model <- holt(train$Open, alpha=0.2, h=test_length)
holt_pred <- predict(holt_model, test_length)

plot(holt_pred$mean[1:10], type="l", col="green")
lines(test$Open[1:10], type="l", col="red")

# doesnt work well

#--------
#exponential smoothing

expon1 <- ses(train$Open, alpha=0.95, h=3)
expon2 <- ses(train$Open, alpha=0.5, h=5)
expon3 <- ses(train$Open, alpha=0.1, h=7)
plot(fitted(expon), col="red")
lines(train$Open, col="green")
lines(fitted(expon2), col="blue")
lines(fitted(expon3), col="cyan")


