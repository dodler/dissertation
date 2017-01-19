library(ggplot2)

setwd('/home/lyan//Documents/dissertation/dissertation/data/')

data <- read.csv('goog.csv')

CUT_UP = 0.7
train_length <- floor(length(data$Date) * 0.7)
data_length <- length(data$Date)

train <- data[0:train_length,]
test <- data[train_length:data_length,]

ggplot(data=data, aes(x=Date, y=Open, group=1)) +
  geom_line() + 
  geom_point()

arima_model <- arima(data$Open, order=c(2,0,2))
print (predict(arima_model,10))
