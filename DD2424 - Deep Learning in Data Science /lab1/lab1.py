import numpy as np
import cPickle

NUM_LABELS = 10
LAMBD = 0.01

"""
Read images from the file
"""
def unpickle(file):
    fo = open(file, 'rb')
    dict = cPickle.load(fo)
    fo.close()
    return dict


"""
X contains the image pixel data, has size d*N, is of type double or
single and has entries between 0 and 1. N is the number of images
(10000) and d the dimensionality of each image (3072=32*32*3).

Y is K*N (K= # of labels = 10) and contains the one-hot representation
of the label for each image.

y is a vector of length N containing the label for each image. A note
of caution. CIFAR-10 encodes the labels as integers between 0-9 but
Matlab indexes matrices and vectors starting at 1. Therefore it may be
easier to encode the labels between 1-10.

"""
def load_batch():
	data = unpickle('data_batch_1')
	X = np.matrix(data.get('data')).T #
	X = X.astype(float) / 255
	Y = np.zeros((NUM_LABELS,X.shape[1]))
	y = np.array(data.get('labels')).reshape(-1,1)
	Y = ((np.arange(y.max()+1) == y[:,:]).astype(int)).T
	return [X,Y,y]


"""
W has size K*d and b is K*1. Initialize each entry to have Gaussian
random values with zero mean and standard deviation .01. You should
use the Matlab function randn to create this data.

"""
def init_model(dimOfImage):
	W = 0.01 * np.random.randn(NUM_LABELS, dimOfImage) + 0
	b = 0.01 * np.random.randn(NUM_LABELS, 1) + 0
	return [W,b]

"""
Each column of X corresponds to an image and it has size d*n.
W and b are the parameters of the network.
Each column of P contains the probability for each label for the image
in the corresponding column of X. P has size K*n.

"""
def evaluate_classifier(X,W,b):
	scores = W*X+b
	return (np.exp(scores) / np.sum(np.exp(scores), axis=0))

"""
Each column of X corresponds to an image and X has size d*n.

Each column of Y (K*n) is the one-hot ground truth label for the corresponding
column of X or Y is the (1*n) vector of ground truth labels.

J is a scalar corresponding to the sum of the loss of the network's
predictions for the images in X relative to the ground truth labels and
the regularization term on W.
"""
def compute_cost(X,Y,P,W):
	cross_entropy = np.sum(-np.log(Y.T*P))
	num_imgs = X.shape[1]
	cost = (1/num_imgs)* cross_entropy + LAMBD*np.sum(W*W)
	return cost


"""

Each column of X corresponds to an image and X has size d*n.
Y is the vector of ground truth labels of length n.
acc is a scalar value containing the accuracy.

"""

def compute_accuracy(y,P):
	preds = np.argmax(P,axis=0).T
	correct = 0
	for i in range(0,preds.size):
		if(y[i]==preds[i]):
			correct +=1
	acc = float(correct)/y.shape[0]
	return acc

"""
Each column of X corresponds to an image and it has size d*n.
Each column of Y (K*n) is the one-hot ground truth label for the corresponding
column of X.
Each column of P contains the probability for each label for the image in the corresponding column of X. P has size K*n.

grad W is the gradient matrix of the cost J relative to W and has size K*d.
grad b is the gradient vector of the cost J relative to b and has size K*1.
"""

def compute_gradients():



[X,Y,y] = load_batch()
[W,b] = init_model(X.shape[0])

print(W)
print(b)

P = evaluate_classifier(X,W,b)

#cost = compute_cost(X,Y,P,W)

acc = compute_accuracy(y,P)

print(acc)