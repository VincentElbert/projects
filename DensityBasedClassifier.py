import scipy.io as sio
from math import sqrt
import numpy as np
import matplotlib.pyplot as plt


face_trainX = sio.loadmat("C:/Users/vince/Desktop/task/CSC4008/hw/ATNT face/ATNT face/trainX")['trainX']
face_testX = sio.loadmat("C:/Users/vince/Desktop/task/CSC4008/hw/ATNT face/ATNT face/testX.mat")['testX']
face_trainY = sio.loadmat("C:/Users/vince/Desktop/task/CSC4008/hw/ATNT face/ATNT face/trainY.mat")['trainY']
face_testY = sio.loadmat("C:/Users/vince/Desktop/task/CSC4008/hw/ATNT face/ATNT face/testY.mat")['testY']

hand_trainX = sio.loadmat("C:/Users/vince/Desktop/task/CSC4008/hw/Binalpha handwritten/Binalpha handwritten/trainX.mat")['trainX']
hand_testX = sio.loadmat("C:/Users/vince/Desktop/task/CSC4008/hw/Binalpha handwritten/Binalpha handwritten/testX.mat")['testX']
hand_trainY = sio.loadmat("C:/Users/vince/Desktop/task/CSC4008/hw/Binalpha handwritten/Binalpha handwritten/trainY.mat")['trainY']
hand_testY = sio.loadmat("C:/Users/vince/Desktop/task/CSC4008/hw/Binalpha handwritten/Binalpha handwritten/testY.mat")['testY']

def euclideanDistance(col1, col2):
    distance = 0
    for i in range (0,len(col1)):
        distance += (int(col1[i]) - int(col2[i]))**2
    return sqrt(distance)

def majorityVote(x):
    vote = 0
    if len(x) >= 1:
        vote = max(set(x), key = x.count)
    return vote

def distance(testX, trainX):
    dist = []
    for i in range(0, testX.shape[1]): 
        dist_i = []
        for j in range(0, trainX.shape[1]):
            dist_i.append(euclideanDistance(testX[:, i], trainX[:, j]))
        dist.append(dist_i)
    dist = np.array(dist)
    return dist

def accuracy(actual, prediction):
    true = 0
    for i in range(0, len(actual)):
        if actual[i] == prediction[i]:
            true += 1
    return (true/len(actual))*100

def predict(data, trainY, r):
    predict = []
    for i in range(0, len(data)):
        data_i = data[i].tolist()
        neighbor = []
        for j in range(0, len(data_i)):
            if data_i[j] <= r:
                neighbor.append(trainY[j])
            else:
                continue
        predict.append(majorityVote(neighbor))
    return predict


def DBC(testX, trainX, testY, trainY):
    dist = distance(testX, trainX)
    output = trainY[0].tolist()
    r1 = int(np.min(dist))
    r2 = int(np.quantile(dist, 0.25))
    actual = testY[0].tolist()
    acc = []
    optimal_pred = []
    optimal_acc = 0
    optimal_r = 0
    acc_r = 0
    for r in range(r1, r2):
        pred = predict(dist, output, r)
        acc_r = accuracy(actual, pred)
        if acc_r > optimal_acc:
            optimal_acc = acc_r
            optimal_pred.append(pred)
            optimal_r = r
        acc.append(acc_r)
    plt.plot(np.arange(r1, r2, 1), acc)
    plt.show()
    print('The highest accuracy reached is: ', optimal_acc)
    print(' With optimal epsilon :', optimal_r)


print('The Density-based classifier result of ATNT is: ')
DBC(face_testX, face_trainX, face_testY, face_trainY)
print('The Density-based classifier result of Binalpha is: ')
DBC(hand_testX, hand_trainX, hand_testY, hand_trainY)

