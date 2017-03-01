library(ggplot2)
library(forecast)

#setwd("C:/Users/SBT-Lyan-AI/Documents/dissertation/data/")
setwd('/home/lyan/Documents/dissertation/dissertation/data')

CUT_UP = 0.9
train_length <- floor(length(data$Date) * CUT_UP)
data_length <- length(data$Date)
test_length <- data_length - train_length

train <- data[1:train_length,]
test <- data[train_length:data_length,]

arima_model <- arima(train$Open, order=c(2,0,2))
predicted <- predict(arima_model,5)



#------------------
#predicting

values_to_predict = 5
prediction = list(test_length)


lastSuccess = 0

for(i in 1:(test_length - values_to_predict)){
  
  lastSuccess = i
  
  tryCatch({
    arima_model <- arima(test[i:(i+values_to_predict),]$Close, order=c(0,0,3))
  }, error=function(e){})
  
  pred <- predict(arima_model,1)$pred[1]
  prediction[i] <- pred
}
prediction <- as.data.frame(as.numeric(prediction))
#-------------
#plotting
test_values <- as.data.frame(test[1:lastSuccess,]$Close)
index <- as.data.frame(1:lastSuccess)

expon <- as.data.frame(fitted(ses(test[1:lastSuccess,]$Close, alpha=0.1, h=5)))

result_data <- as.data.frame(cbind(test_values, prediction, index, expon))
colnames(result_data) <- c("test", "pred", "index", "expon")

ggplot(data=result_data, aes(x=index, group=1)) +
  geom_line(data=result_data, aes(y=pred), color="red") + 
  geom_line(data=result_data, aes(y=test), color="green")+
  geom_line(data=result_data, aes(y=expon), color="cyan")

#-------
#holt winters

holt_model <- holt(train$Open, alpha=0.2, h=3)
holt_pred <- predict(holt_model, 3)

<<<<<<< HEAD
plot(holt_pred$mean, type="l", col="green")
lines(test$Open, type="l", col="red")

# doesnt work well

#--------
#exponential smoothing

ses_predict_step = 1
ses_models_number = 50

ses_model_step = 0.99/(ses_models_number+1)
cur_step = 0.99

values <- fitted(ses(train$Open, alpha=cur_step, h=ses_predict_step))

for (i in 2:ses_models_number){
  
  cur_step <- cur_step - ses_model_step
  
  print(cur_step)
  
  values <- rbind( values,
                   fitted(ses(train$Open, alpha=cur_step, h=ses_predict_step)))
}

values <- rbind(values, train$Open)
=======
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

>>>>>>> e2040b5b9383198979733f42599030087203e06e

im <- image(values)

