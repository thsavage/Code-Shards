import time
import sys

import numpy as np
import tensorflow as tf
import pandas as pd

import statsmodels.api as sm
import statsmodels.formula.api as smf
from sklearn.model_selection import train_test_split

from keras.models import Sequential
from keras.layers import Dense
from keras.layers import LSTM
from keras import optimizers
from keras.callbacks import LearningRateScheduler
from keras.callbacks import ModelCheckpoint
import keras.backend as K
from keras.models import load_model

import pandas_datareader.data as web
import fix_yahoo_finance

def getAppleData():
    # Grab 10 years of Apple and NASDAQ data.  
    # Note that the old Yahoo Finance API was deprecated.  (Thanks, Verizon!  That Marissa pay-out was steep.)
    # Ran Aroussi has developed a fix that saves accessibility to Yahoo Finance. Thanks, Ran! Great stuff. 
    start, end = "2006-01-01", "2015-12-31"
    aapl_all = web.get_data_yahoo("aapl", start=start, end=end)
    nasdaq_all = web.get_data_yahoo("^ixic", start=start, end=end)
    aapl = aapl_all['Adj Close']
    nasdaq = nasdaq_all['Adj Close']
    
    # Calculate daily returns.
    aapl_returns = np.log(aapl / aapl.shift(1))
    nasdaq_returns = np.log(nasdaq / nasdaq.shift(1))
    aapl_returns = aapl_returns.dropna()
    nasdaq_returns = nasdaq_returns.dropna()
    aapl_returns = pd.DataFrame(aapl_returns)
    nasdaq_returns = pd.DataFrame(nasdaq_returns)
    
    data = pd.merge(nasdaq_returns, aapl_returns, left_index=True, right_index=True)
    data.rename(columns={'Adj Close_x':'nasdaq', 'Adj Close_y':'aapl'}, inplace=True)
    return data

def getAmazonData():
    # Grab Amazon and Fama-French 3 Factor data
    start, end = "1998-01-01", "2017-3-30"
    amzn_all = web.get_data_yahoo('amzn', start, end)
    ff5f = pd.read_csv('ff5f.csv', index_col='Date')
    ff5f.set_index(pd.to_datetime(ff5f.axes[0].astype(str)), inplace=True)
    ff3f = ff5f.rename(columns = {'Mkt-RF':'Mkt_RF'}) / 100
    amzn_all['AMZN_r'] = np.log(amzn_all['Close'] / amzn_all['Close'].shift(1)).dropna()
    data = pd.merge(amzn_all, ff3f, left_index=True, right_index=True).dropna()
    return data

class MLPModel:
    name = 'MLP'
    
    @staticmethod
    def create(layers=[500,1000,1000,1000,500], input_dim=1):
        model = Sequential()
        for u in layers:
            model.add(Dense(units=u, activation='relu', input_dim=input_dim))
        model.add(Dense(units=1))
        model.compile(loss='mse', optimizer='adam', metrics=['mse'])
        return model

    @staticmethod
    def prepareData(X, Y, **kwargs):
        return X, Y

class LSTMModel:
    name = 'LSTM'

    @staticmethod
    def create(blocks=4, look_back=1, input_dim=1):
        shape = (input_dim,look_back+1)
        model = Sequential()
        model.add(LSTM(blocks, input_shape=shape))
        model.add(Dense(units=1))
        model.compile(loss='mse', optimizer='adam', metrics=['mse'])
        return model
    
    @staticmethod
    def createDataset(X, Y, look_back=1):
        dataX, dataY = [], []
        for i in xrange(len(X)-look_back-1):
            a = X[i:(i+look_back+1)]
            dataX.append(a)
            dataY.append(Y[i+look_back])
        return np.array(dataX), np.array(dataY)
    
    @staticmethod
    def prepareData(X, Y, **kwargs):
        X, Y = LSTMModel.createDataset(X, Y, kwargs['look_back'])
        return X.reshape(X.shape[0], X.shape[2], X.shape[1]), Y

def evalModels(data, 
               models=[],
               epochs=20, batch_size=100,
               nIter=9, testSplit=0.2,
               xVars=['nasdaq'], yVar='aapl',
              ):
    allResults = []

    # Prep data
    datatrain, datatest = train_test_split(data, test_size = testSplit, random_state=1114)
    rawTrainX = datatrain[xVars].as_matrix()
    rawTrainY = datatrain[yVar].as_matrix()
    rawTestX = datatest[xVars].as_matrix()
    rawTestY = datatest[yVar].as_matrix()
    datatest = datatest.copy()
        
    # Run OLS
    print ('Run OLS')
    startTime = time.time()
    sumMSE = 0
    for i in xrange(nIter):
        print('Iteration %d/%d' % (i+1, nIter)),
        mod = smf.ols(formula='%s ~ %s' % (yVar, ' + '.join(xVars)), data = datatrain).fit()
        fitted = mod.predict(exog = datatest)
        mse = ((datatest[yVar] - fitted)**2).mean()
        print('- the MSE is %f' % mse)

        sumMSE += mse
    print ('The average MSE is %f' % (sumMSE/nIter))
    print ('The average running time is: %0.2f seconds\n' % ((time.time()-startTime)/nIter))
    allResults.append((sumMSE, sumTime))
    
    # Run model
    np.random.seed(1115)
    for (modelClass, modelParams) in models:
        modelName = modelClass.name
        trainX, trainY = modelClass.prepareData(rawTrainX, rawTrainY, **modelParams)
        testX, testY = modelClass.prepareData(rawTestX, rawTestY, **modelParams)
        print ('Run %s - %s' % (modelName, modelParams))
        startTime = time.time()
        sumMSE = 0
        for i in xrange(nIter):
            print('Iteration %d/%d' % (i+1, nIter)),
            model = modelClass.create(input_dim=trainX.shape[1], **modelParams)
            model.fit(trainX, trainY, epochs=epochs, batch_size=batch_size, verbose=0)
            target = model.predict(testX)
            colName = '%s %s' % (modelName, i)
            padSize = len(datatest)-len(target)
            if padSize>0:
                target = np.concatenate((datatest[yVar].as_matrix()[:padSize].reshape(padSize,1), target), axis=0)
            datatest[colName] = target
            mse = ((datatest[yVar] - datatest[colName])**2).mean()
            print('- the MSE is %f' % mse)
            sumMSE += mse
        print ('The average MSE is %f' % (sumMSE/nIter))
        print ('The average running time is: %0.2f seconds\n' % ((time.time()-startTime)/nIter))
        allResults.append((sumMSE, sumTime))

    return allResults

if __name__=='__main__':
    if len(sys.argv)<2:
        print 'Usage: %s <NUM_ITERATIONS>' % sys.argv[0]
        sys.exit(0)
    #dataApple = getAppleData()
    dataAmazon = getAmazonData()
    nIter = int(sys.argv[1])
    with tf.device('/cpu:0'):
        results = evalModels(dataAmazon, nIter=nIter,
                             models=((MLPModel, {'layers': [500,1000,1000,1000,500]}),
                                     (LSTMModel, {'blocks': 10, 'look_back': 30}),
                                     (LSTMModel, {'blocks': 10, 'look_back': 90}),
                                     ),
                             epochs=10, batch_size=512,
                             xVars= ['RF', 'Mkt_RF', 'SMB', 'HML'], yVar='AMZN_r')
    mse, time = map(np.array, zip(*results))
    stats = np.concatenate((mse, time, mse/nIter, time/nIter), axis=0)
    print ','.join(map(str, stats))
