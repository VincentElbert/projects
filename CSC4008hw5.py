import random
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
from sklearn.svm import SVC

def generate_random_dataset(size):
    x1 = []
    x2 = []
    y = []
    for i in range(size // 2):
        # class -1
        x1.append(np.random.uniform(0, 3))
        x2.append(np.random.uniform(0, 3))
        y.append(-1)
        # class 1
        x1.append(np.random.uniform(2, 5))
        x2.append(np.random.uniform(2, 5))
        y.append(1)
    df_x1 = pd.DataFrame(data=x1)
    df_x2 = pd.DataFrame(data=x2)
    df_classes = pd.DataFrame(data=y)
    data_frame = pd.concat([df_x1, df_x2, df_classes], ignore_index=True, axis=1)
    data_frame = np.array(data_frame)
    return data_frame

def fitSVCmodel(features, y, C):
    model = SVC(kernel = 'linear', C = C)
    model.fit(features, y)
    return model

def getAlphas(model):
    alpha_i = np.abs(model.dual_coef_)
    return alpha_i

def getSV(model):
    sv = model.support_vectors_
    return sv

def getDecisionFunction(model):
    function = model.decision_function
    return function

# plot the hyperplane along with the margins
def plotLines(function):
    ax = plt.gca()
    xlim = ax.get_xlim()
    ylim = ax.get_ylim()
    xx = np.linspace(xlim[0], xlim[1], 30)
    yy = np.linspace(ylim[0], ylim[1], 30)
    YY, XX = np.meshgrid(yy, xx)
    xy = np.vstack([XX.ravel(), YY.ravel()]).T
    Z = function(xy).reshape(XX.shape)
    plt.contour(
        XX, YY, Z, colors="k", levels=[-1, 0, 1], 
        alpha=0.5, linestyles=["--", "-", "--"]
    )

# view the values of alpha and classify it on the plot
# red circles for alpha = 0, and black squared for alpha = C
def getAlpha_i(features, alpha_i, sv, C):
    index_sv = []
    alpha_C = []
    for i in range(0, len(sv)):
        index = np.where((features == sv[i]).all(axis = 1))
        index_sv.append(index[0])
        if alpha_i[0, i] == C:
            alpha_C.append(sv[i])
    alpha_C = np.array(alpha_C)

    alpha_0 = []
    for i in range(0, len(features)):
        if i in index_sv:
            continue
        else:
            alpha_0.append(features[i])
    alpha_0 = np.array(alpha_0)

    print('Features with alpha = 0:', len(alpha_0))
    print('Features with alpha = C:', len(alpha_C))
    print('Features with ksi > 0:', len(alpha_C))

    plt.scatter(
        alpha_C[:, 0], alpha_C[:, 1],
        s = 25, marker = 's', linewidth = 1,
        facecolors = 'none', edgecolors = 'black',
        )

    plt.scatter(
        alpha_0[:, 0], alpha_0[:, 1],
        s = 25, linewidth = 1,
        facecolors = 'none', edgecolors = 'red',
        )

def SVM(size, C):
    size = 200
    dataset = generate_random_dataset(size)
    features = dataset[:,(0,1)]
    y = dataset[:,2]
    
    print('SVM for C = ', C, 'with datasize =', size)

    sns.scatterplot(
        x=features[:,0], y=features[:,1], 
        hue=y, palette=['blue', 'green']
        )

    model = fitSVCmodel(features, y, C)
    alpha_i = getAlphas(model)
    sv = getSV(model)
    function = getDecisionFunction(model)
    getAlpha_i(features, alpha_i, sv, C)
    plotLines(function)


    plt.show()

SVM(size = 200, C = 0.01)
SVM(size = 200, C = 0.1)