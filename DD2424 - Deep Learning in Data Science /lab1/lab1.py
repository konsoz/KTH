import numpy as np
import cPickle
from scipy.optimize import check_grad
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches

NUM_LABELS = 10
LAMBD = 0
ETA = 0.01
N_BATCH = 100
N_EPOCHS = 40

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
def load_one_batch():
	data = unpickle('data_batch_1')
	X = np.matrix(data.get('data')).T #
	X = X.astype(float) / 255
	Y = np.zeros((NUM_LABELS,X.shape[1]))
	y = np.array(data.get('labels')).reshape(-1,1)
	Y = ((np.arange(y.max()+1) == y[:,:]).astype(int)).T
	return [X,Y,y]

def load_batches():
	data1 = unpickle('data_batch_1')
	data2 = unpickle('data_batch_2')
	data = np.concatenate((data1.get('data'),data2.get('data')))
	X = np.matrix(data).T #
	X = X.astype('float_') / 255
	Y = np.zeros((NUM_LABELS,X.shape[1]))
	y_1 = data1.get('labels')
	y_2 = data2.get('labels')
	y = np.array(y_1+y_2).reshape(-1,1)
	Y = ((np.arange(y.max()+1) == y[:,:]).astype(int)).T
	return [X,Y,y]


def load_test():
	test_data = unpickle('test_batch')
	Xtest = np.matrix(test_data.get('data')).T
	Xtest = Xtest.astype('float64') / 255
	y_test = np.array(test_data.get('labels')).reshape(-1,1)
	Ytest = ((np.arange(y_test.max()+1) == y_test[:,:]).astype(int)).T
	return Xtest,y_test, Ytest

"""
W has size K*d and b is K*1. Initialize each entry to have Gaussian
random values with zero mean and standard deviation .01. You should
use the Matlab function randn to create this data.

"""
def init_model(dimOfImage):
	np.random.seed(400)
	W = np.random.normal(0,0.01,(NUM_LABELS, dimOfImage))
	b = np.random.normal(0,0.01,(NUM_LABELS, 1))
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
	cross_entropy = 0
	for i in range(0,X.shape[1]):
		y = Y[:,i]
		p = P[:,i]
		ent = -np.log(y.T*p)
		cross_entropy += ent
	num_imgs = X.shape[1]
	cost = (1/float(num_imgs))* cross_entropy + LAMBD*np.sum(W*W)
	return cost


"""

Each column of X corresponds to an image and X has size d*n.
Y is the vector of ground truth labels of length n.
acc is a scalar value containing the accuracy.

"""

def compute_acc(y,P):
	preds = np.argmax(P,axis=0).T
	diff = preds - y
	num_correct = diff[diff == 0].shape[1]
	num_tot = y.shape[0]
	acc = num_correct / float(num_tot)
	return acc*100

"""
Each column of X corresponds to an image and it has size d*n.
Each column of Y (K*n) is the one-hot ground truth label for the corresponding
column of X.
Each column of P contains the probability for each label for the image in the corresponding column of X. P has size K*n.

grad W is the gradient matrix of the cost J relative to W and has size K*d.
grad b is the gradient vector of the cost J relative to b and has size K*1.
"""

def compute_gradients(X, Y, P, W,b):
	gradW = np.zeros((Y.shape[0],X.shape[0]))
	gradb = np.zeros((Y.shape[0],1))
	for i in range(0,X.shape[1]):
		corresponding_softmax_probs = P[:,i]
		diag_p = np.diagflat(corresponding_softmax_probs) 
		g = -((Y[:,i] / (Y[:,i]*P[:,i])) * (diag_p-corresponding_softmax_probs*corresponding_softmax_probs.T))
		gradb += g.T
		gradW += g.T*X[:,i].T
	gradW /= X.shape[1]
	gradb /= X.shape[1]
	gradW = gradW + 2*LAMBD*W  
	return [gradW,gradb]




def get_mini_batches(Xtrain,Ytrain):
	Xbatches = []
	Ybatches = []
	for j in range(1,(Xtrain.shape[1]/N_BATCH)+1):
		j_start = (j-1)*N_BATCH
		j_end = j*N_BATCH
		Xbatch = Xtrain[:,j_start:j_end]
		Ybatch = Ytrain[:,j_start:j_end]
		Xbatches.append(Xbatch)
		Ybatches.append(Ybatch)
	return [Xbatches,Ybatches]

def compute_grads_num(X, Y, W, b, lbda, h):
    # Numerical check for gradients
    no = W.shape[0]
    d = X.shape[1]

    grad_W = np.zeros(W.shape)
    grad_b = np.zeros(no)

    P = evaluate_classifier(X,W,b)

    c = compute_cost(X, Y, P,W)

    for i in range(no):
        b_try = np.copy(b)
        b_try[i] += h
        P = evaluate_classifier(X,W,b)
        c2 = compute_cost(X, Y, P, W)
        grad_b[i] = (c2 - c) / h

    for i in range(W.shape[0]):
        for j in range(W.shape[1]):
            W_try = np.copy(W)
            W_try[i, j] = W_try[i, j] + h;
            P = evaluate_classifier(X,W,b)
            c2 = compute_cost(X, Y, P,W_try)
            
            grad_W[i, j] = (c2 - c) / h

    return grad_W, grad_b

def train_softmax_reg():
	X,Y,y = load_one_batch()
	W,b = init_model(X.shape[0])
	Xtest, ytest, Ytest = load_test()
	Xbatches, Ybatches = get_mini_batches(X,Y)
	costs_train = []
	costs_test = []
	
	for epoch in range(0,1):
		for batch in range(0,len(Xbatches)):	 
			P = evaluate_classifier(Xbatches[batch],W,b)
			gradW, gradB = compute_gradients(Xbatches[batch],Ybatches[batch],P,W,b)
			W = W - ETA*gradW
			b = b - ETA*gradB
		P_test = evaluate_classifier(Xtest,W,b)
		P_train = evaluate_classifier(X,W,b)
		cost_train = compute_cost(X,Y,P_train,W)
		cost_test = compute_cost(Xtest,Ytest,P_test,W)
		print("cost train: %f" %cost_train)
		print("cost test: %f" %cost_test)
		print("accuracy test: %f" %compute_acc(ytest,P_test))
		print("accuracy train: %f" %compute_acc(y,P_train))
		costs_train.append(cost_train[0,0])
		costs_test.append(cost_test[0,0])

	epochs_arr = np.arange(0, N_EPOCHS).tolist()
	"""
	plt.plot(epochs_arr, costs_train, 'r-',label='training loss')
	plt.plot(epochs_arr, costs_test, 'b-',label='validation loss')
	plt.legend(loc='upper center', shadow=True)
	plt.xlabel('Epoch')
	plt.ylabel('Loss')
	plt.show()
	"""
	img = W[1,:].reshape(32,32)
	plt.imshow(img,origin='lower',interpolation='nearest',aspect='auto')
	plt.show()

train_softmax_reg()