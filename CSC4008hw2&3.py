import scipy.io as sio
import pandas as pd
from math import sqrt
import numpy as np
import matplotlib.pyplot as plt

# This is to open the files and assign them to each corresponding variables
facetrainX = sio.loadmat("ATNT face/ATNT face/trainX.mat")['trainX']
facetestX = sio.loadmat("ATNT face/ATNT face/testX.mat")['testX']
faceTrainY = sio.loadmat("ATNT face/ATNT face/trainY.mat")['trainY']
faceTestY = sio.loadmat("ATNT face/ATNT face/testY.mat")['testY']

handtrainX = sio.loadmat("Binalpha handwritten/Binalpha handwritten/trainX.mat")['trainX']
handtestX = sio.loadmat("Binalpha handwritten/Binalpha handwritten/testX.mat")['testX']
handTrainY = sio.loadmat("Binalpha handwritten/Binalpha handwritten/trainY.mat")['trainY']
handTestY = sio.loadmat("Binalpha handwritten/Binalpha handwritten/testY.mat")['testY']

def euclidean_dist(col1, col2):
    euclidDist = 0
    for i in range (0,len(col1)):
        euclidDist += (int(col1[i]) - int(col2[i]))**2
    return sqrt(euclidDist)

def majority_vote(x):
    vote = 0
    if len(x) >= 1:
        vote = max(set(x), key = x.count)
    return vote

def distance(testX, trainX):
    dist = []
    for i in range(0, testX.shape[1]): 
        dist_i = []
        for j in range(0, trainX.shape[1]):
            dist_i.append(euclidean_dist(testX[:, i], trainX[:, j]))
        dist.append(dist_i)
    dist = np.array(dist)
    return dist

def accuracy(actual, prediction):
    true = 0
    for i in range(0, len(actual)):
        if actual[i] == predict[i]:
            true += 1
    accuracy.append(true/len(actual))

def predict(dist, trainY, r):
    predict = []
    output = trainY[0].tolist()
    for i in range(0, len(dist)):
        dist_list = dist[i].tolist()
        prob_class = []
        for j in range(0, len(dist_list)):
            if dist_list[j] <= r:
                prob_class.append(output[j])
            else:
                continue
        predict.append(majority_vote(prob_class))

def DBC(testX, trainX, testY, trainY):
    prediction = []
    trainY_list = trainY[0].tolist()
    dist = distance(testX, trainX)
    r1 = int(np.min(dist))
    r2 = int(np.min(dist))
    for r in range(r1, r2):
        predict = []
        for i in range(0, len(dist)):
            dist_list = dist[i].tolist()
            prob_class = []
            for j in range(0, len(dist_list)):
                if dist_list[j] <= r:
                    prob_class.append(trainY_list[j])
                else:
                    continue
            predict.append(majority_vote(prob_class))
        prediction.append(predict)
    prediction = np.array(prediction)
    acc = []
    actual = testY[0].tolist()
    for i in range(0, len(prediction)):
        true = 0
        predict_i = prediction[i].tolist()
        for j in range(0, len(actual)):
            if actual[j] == predict_i[j]:
                true += 1
        acc.append(true/len(actual))
    plt.plot(acc)
    plt.show
    
DBC(facetestX, facetrainX, faceTestY, faceTrainY)