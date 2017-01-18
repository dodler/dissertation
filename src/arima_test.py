import os.path

from pandas import datetime
import matplotlib.pyplot as plt
import pandas as pd

import numpy as np
from pandas.tools.plotting import autocorrelation_plot
from sklearn.metrics import mean_squared_error

from statsmodels.tsa.arima_model import ARIMA

ewma = pd.stats.moments.ewma

data_path = '/../data/'


def parser(date):
    return pd.to_datetime(date, format='%Y-%m-%d')


sber_data = pd.read_csv(os.path.dirname(__file__) + data_path + 'sber.csv', header=0,
                        parse_dates=[0], index_col=0, squeeze=True, date_parser=parser)

open_values = sber_data['Open'].values

CROP_RATIO = 0.66

size = int(len(open_values) * CROP_RATIO)

train, test = open_values[0:size], open_values[size:len(open_values)]

history = [x for x in train]

predictions = list()

for t in range(len(test)):
    model = ARIMA(history, order=(5,1,0))
    model_fit = model.fit(disp=0)
    output = model_fit.forecast()
    yhat = output[0]
    predictions.append(yhat)
    obs = test[t]
    history.append(obs)

error = mean_squared_error(test, predictions)

plt.plot(test)
plt.plot(predictions, color="red")
plt.show()



