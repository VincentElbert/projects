import scipy.io as sio
from math import sqrt
import numpy as np

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
        distance += (col1[i] - col2[i])**2
    return sqrt(distance)

def accuracy(actual, prediction):
    true = 0
    for i in range(0, len(actual)):
        if actual[i] == prediction[i]:
            true += 1
    return (true/len(actual))*100

def getCentroid(a):
    centroid = []
    a = np.array(a)
    for i in range(0, a.shape[1]):
        data = a[:, i].tolist()
        centroid.append(sum(data)/len(data))
    return centroid

def locate(trainX, trainY, classes):
    output = trainY[0].tolist()
    centroid_position = []
    for i in range(0, len(classes)):
        location = []
        for j in range(0, len(output)):
            if classes[i] == output[j]:
                location.append(trainX[:, j])
        centroid_position.append(getCentroid(location))
    centroid_position = np.array(centroid_position)
    return centroid_position

def distance(testX, position):
    dist = []
    for i in range(0, testX.shape[1]): 
        dist_i = []
        for j in range(0, len(position)):
            dist_i.append(euclideanDistance(testX[:, i], position[j]))
        dist.append(dist_i)
    dist = np.array(dist)
    return dist

def predict(dist, classes):
    predict = []
    for i in range(0, len(dist)):
        dist_list = dist[i].tolist()
        dist_sort = dist_list.copy()
        dist_sort.sort()
        which_class = dist_list.index(dist_sort[0])
        predict.append(classes[which_class])
    return predict

def centroidCLF(testX, trainX, testY, trainY):
    actual = testY[0].tolist()
    classes = np.unique(trainY)
    position = locate(trainX, trainY, classes)
    dist = distance(testX, position)
    pred = predict(dist, classes)
    print(accuracy(actual, pred))

print('for ATNT file, the Centroid Classifier achieves an accuracy of :')
centroidCLF(face_testX, face_trainX, face_testY, face_trainY)

print('for Binalpha file, the Centroid Classifier achieves an accuracy of :')
centroidCLF(hand_testX, hand_trainX, hand_testY, hand_trainY)
