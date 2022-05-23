import scipy.io as sio
import pandas as pd
from math import sqrt

# This is to open the files and assign them to each corresponding variables
faceTrainX = sio.loadmat("ATNT face/ATNT face/trainX.mat")
faceTestX = sio.loadmat("ATNT face/ATNT face/testX.mat")
faceTrainY = sio.loadmat("ATNT face/ATNT face/trainY.mat")
faceTestY = sio.loadmat("ATNT face/ATNT face/testY.mat")

handTrainX = sio.loadmat("Binalpha handwritten/Binalpha handwritten/trainX.mat")
handTestX = sio.loadmat("Binalpha handwritten/Binalpha handwritten/testX.mat")
handTrainY = sio.loadmat("Binalpha handwritten/Binalpha handwritten/trainY.mat")
handTestY = sio.loadmat("Binalpha handwritten/Binalpha handwritten/testY.mat")

# Change the variable form into data frame
faceTrainX = pd.DataFrame(faceTrainX["trainX"])
faceTestX = pd.DataFrame(faceTestX["testX"])
faceTrainY = pd.DataFrame(faceTrainY["trainY"])
faceTestY = pd.DataFrame(faceTestY["testY"])

handTrainX = pd.DataFrame(handTrainX["trainX"])
handTestX = pd.DataFrame(handTestX["testX"])
handTrainY = pd.DataFrame(handTrainY["trainY"])
handTestY = pd.DataFrame(handTestY["testY"])

#calculate the euclidean distance (distance of one point to another)
def euclidean_dist(col1, col2):
    euclidDist = 0
    for i in range (0,len(col1)):
        euclidDist += (int(col1[i]) - int(col2[i]))**2
    return sqrt(euclidDist)

# This function assigns vote for each output from the train dataset
# The most vote an output get, it will then assign the most output
def majority_vote(output):
    vote = {}
    for value in output:
        if (value in vote):
            vote[value] += 1
        else:
            vote[value] = 1
    return max(vote, key = vote.get)

# Calculate accuracy between the predicted and the actual dataset, in this case testY
def accuracy(actual, prediction):
    true = 0
    for i in range(len(actual)):
        if actual[i] == prediction[i]:
            true += 1
    return true/len(actual)*100

# Find the nearest neighbors of the point to be predicted according to chosen k
def nearest(trainX, testX, k):
	distances = []
	for i in range(len(trainX.iloc[0])):
		distances.append(euclidean_dist(testX, trainX.iloc[:, i]))
	neighbors = sorted(range(len(distances)), key = lambda n: distances[n])[0:k]
	return neighbors

# Predict the output using the majority vote function from before
# This function's job is only to assign the prediction for the testX's output
def predict(trainX, trainY, testX, k):
	neighbors = nearest(trainX, testX, k)
	prediction = majority_vote(trainY[neighbors].values.tolist()[0])
	return prediction

# Assemble all the functions and to repeat to every x in testX
def knn(trainX, trainY, testX, testY, k):
	predictions = []
	for i in range(len(testX.iloc[0])):
		result = predict(trainX, trainY, testX.iloc[:, i], k)
		predictions.append(result)
	acc = accuracy(predictions, testY.values.tolist()[0])
	return acc

## REPORT

# Question ii (The result of the model)
# ATNT face dataset 
# The acccuracy achieved by 3-NN is 93.75%
print("The model achieved by 3-Nearest-Neighbor for the ATNT face data has accuracy of", knn(faceTrainX, faceTrainY, faceTestX, faceTestY, 3))
#The acccuracy achieved by 5-NN is 93.75%
print("The model achieved by 5-Nearest-Neighbor for the ATNT face data has accuracy of", knn(faceTrainX, faceTrainY, faceTestX, faceTestY, 5))
# The acccuracy achieved by 7-NN is 93.75%
print("The model achieved by 7-Nearest-Neighbor for the ATNT face data has accuracy of", knn(faceTrainX, faceTrainY, faceTestX, faceTestY, 7))
# The acccuracy achieved by 9-NN is 81.25%
print("The model achieved by 9-Nearest-Neighbor for the ATNT face data has accuracy of", knn(faceTrainX, faceTrainY, faceTestX, faceTestY, 9))

# Question i (which k I choose)
# ATNT face dataset
# From the result, I believe that 5-NN is the best model for ATNT face dataset.
# This is due to the fact that choosing k = 3, 5, and 7 yields the same accuracy.
# If we choose 3, the model has larger variance. While choosing 7 results in larger bias.
# Therefore, choosing k as 5 leads to less variance and less bias (the best of both worlds).
# But, why not k = 1? since 1 will overfit the model although it will yield higher accuracy.

# Question ii (The result of the model)
# Binalpha handwritten dataset
# The accuracy achieved by 3-NN is 71.7949%
print("The model achieved by 3-Nearest-Neighbor for the Binalpha handwritten data has accuracy of", knn(handTrainX, handTrainY, handTestX, handTestY, 3))
# The acccuracy achieved by 5-NN is 72.2222%
print("The model achieved by 5-Nearest-Neighbor for the Binalpha handwritten data has accuracy of", knn(handTrainX, handTrainY, handTestX, handTestY, 5))
# The acccuracy achieved by 7-NN is 68.8034%
print("The model achieved by 7-Nearest-Neighbor for the Binalpha handwritten data has accuracy of", knn(handTrainX, handTrainY, handTestX, handTestY, 7))
# The acccuracy achieved by 9-NN is 70.51282051282051%
print("The model achieved by 9-Nearest-Neighbor for the Binalpha handwritten data has accuracy of", knn(handTrainX, handTrainY, handTestX, handTestY, 9))

# Question i (which k I choose)
# Binalpha handwritten dataset
# From the results, it is obvious that we should choose KNN with k = 5. 
# The model achieved the best accuracy, while relatively not corresponds to large variance and bias.

# Question iii (other observed result)
# From the result we can see that the accuracy achieved by KNN in Binalpha handwritten dataset is not satisfying.
# This might be caused by many things. One of the cause may be that the dataset has no particular pattern and the datapoints
# with different outputs are scattered around. 