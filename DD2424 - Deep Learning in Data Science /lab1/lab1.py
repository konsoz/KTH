import numpy as np
import cPickle
from scipy.optimize import check_grad
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches

NUM_BATCH_FILES = 5

NUM_LABELS = 10
LAMBD = 1
ETA = 0.01
N_BATCH = 100
N_EPOCHS = 40

"""
Train accuracy improvements:

1. Use all training data, this gives around 3% accuracy increase on the validation data (Parameters: lambda = 0, eta = 0.01, n_epocs = 20 ).


2. Play around with decrease eta after each epoch by a factor of 0.9. Gives around 1% accuracy increase (Parameters: lambda = 0, start_eta = 0.01, n_epocs = 40 )
Cost train: 1.719236
Cost validation: 1.739316
Accuracy validation: 40.250000
Accuracy train: 41.512000

3. Train for a longer time and stop once you begin to overfit (Parameters: lambda = 0, start_eta = 0.1, n_epocs = 100 ).
Cost train: 1.627873
Cost validation: 1.715215
Accuracy validation: 41.200000
Accuracy train: 44.922000


"""


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
	data = [unpickle('data_batch_'+ str(i)) for i in range(1,NUM_BATCH_FILES+1)]
	x_data = [batch.get('data') for batch in data] 
	
	X = np.matrix(x_data[0])
	for i in range(1,len(x_data)):
		X = np.concatenate((X,x_data[i]))
	X = (X.astype('float_') / 255).T

	y_data = [batch.get('labels') for batch in data]
	y = np.array(y_data[0])
	for i in range(1,len(y_data)):
		y = np.concatenate((y,y_data[i]))
	y = y.reshape(-1,1)
	
	Y = np.zeros((NUM_LABELS,X.shape[1]))
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
With our CIFAR-10 example, xixi is now [3073 x 1] instead of [3072 x 1] 
- (with the extra dimension holding the constant 1), 
W is now [10 x 3073] instead of [10 x 3072]. 
The extra column that W now corresponds to the bias b.

"""
def score_function_svm(X,W):
	return W*X

"""
In summary, the SVM cost function wants the score of the correct class yiyi to be 
larger than the incorrect class scores by at least by delta. If this is not the case, we will accumulate loss.

NOTE: 
While the expression may look scary when it is written out, 
when you're implementing this in code you'd simply count the number of classes 
that didn't meet the desired margin (and hence contributed to the loss function) 
and then the data vector xixi scaled by this number is the gradient.

"""
def svm_cost(W, X, y):
  # initialize the gradient as zero
  dW = np.zeros(W.shape) 

  # compute the cost and the gradient
  num_classes = W.shape[0]
  num_train = X.shape[1]
  cost = 0.0

  for i in xrange(num_train):
    scores = W.dot(X[:, i])
    correct_class_score = scores[y[i]]
    for j in xrange(num_classes):
      if j == y[i]:
        continue
      margin = scores[j] - correct_class_score + 1
      if margin > 0:
        cost += margin
        # Gradient with respect to other rows, i.e. j!=yi
        dW[j,:] = W[j,:] + X[:, i].T
        # Gradient with respect to one row of W that corresponds to correct class of x_i
        dW[y[i],:] = dW[y[i],:]- X[:, i].T

  cost /= num_train
  dW   /= num_train

  # Add regularization to the cost and gradient.
  cost += LAMBD*np.sum(W*W)
  dW   += 2*LAMBD*W

  return cost, dW

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

def compute_acc_svm(W,X,y):
	y_pred = np.argmax(W.dot(X), 0).T
	diff = y_pred - y
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



"""
Produce mini-batches of the training data into two 
"""
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

def get_mini_batches_svm(Xtrain,y_train):
	Xbatches = []
	y_batches = []
	for j in range(1,(Xtrain.shape[1]/N_BATCH)+1):
		j_start = (j-1)*N_BATCH
		j_end = j*N_BATCH
		Xbatch = Xtrain[:,j_start:j_end]
		y_batch = y_train[j_start:j_end]
		Xbatches.append(Xbatch)
		y_batches.append(y_batch)
	return Xbatches,y_batches

"""
Numerical computation of the gradients. Used for checking of analytical gradients.
"""
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

"""
Train softmax regression classifier
"""

def train_softmax_reg():
	X,Y,y = load_one_batch()
	W,b = init_model(X.shape[0])
	Xtest, ytest, Ytest = load_test()
	Xbatches, Ybatches = get_mini_batches(X,Y)

	costs_train = []
	costs_validation = []
	
	Eta = 0.1

	for epoch in range(0,N_EPOCHS):
		for batch in range(0,len(Xbatches)):
			P = evaluate_classifier(Xbatches[batch],W,b)
			gradW, gradB = compute_gradients(Xbatches[batch],Ybatches[batch],P,W,b)
			W = W - ETA*gradW
			b = b - ETA*gradB
		#Eta = Eta * 0.9
		P_validation = evaluate_classifier(Xtest,W,b)
		P_train = evaluate_classifier(X,W,b)
		cost_train = compute_cost(X,Y,P_train,W)
		cost_validation = compute_cost(Xtest,Ytest,P_validation,W)
		acc_train = compute_acc(y,P_train)
		acc_validation = compute_acc(ytest,P_validation)
		print("Cost train: %f" %cost_train)
		print("Cost validation: %f" %cost_validation)
		print("Accuracy validation: %f" %acc_validation)
		print("Accuracy train: %f" %acc_train)
		#if(np.absolute(cost_validation[0,0]-cost_train[0,0]) > 3.5):
		#	break
		costs_train.append(cost_train[0,0])
		costs_validation.append(cost_validation[0,0])

	return costs_validation,costs_train,W,b


def train_svm():
	X,_,y = load_one_batch()
	# Add extra row with ones for the bias trick
	X = np.append(X,np.ones((1,X.shape[1])),axis=0)
	# Do not need separate bias vector anymore
	W,_ = init_model(X.shape[0])

	X_batches, y_batches = get_mini_batches_svm(X,y)
	
	Xtest, ytest, _ = load_test()
	Xtest = np.append(Xtest,np.ones((1,Xtest.shape[1])),axis=0)
	costs_train = []
	costs_validation = []

	for epoch in range(0,N_EPOCHS):
		for batch in range(0,len(X_batches)):
			cost, dW = svm_cost(W,X_batches[batch],y_batches[batch])
			W += - ETA * dW

		cost_train, _ = svm_cost(W,X,y)
		acc_train = compute_acc_svm(W,X,y)
		print("Cost train: %f" %cost_train)
		print("Accuracy train: %f" %acc_train)
		cost_validation, _ = svm_cost(W,Xtest,ytest)
		acc_validation = compute_acc_svm(W,Xtest,ytest)
		print("Cost validation: %f" %cost_validation)
		print("Accuracy validation: %f" %acc_validation)
		costs_train.append(cost_train)
		costs_validation.append(cost_validation)




def display(images, rows, cols):
    fig = plt.figure(1, (4., 4.))
    grid = ImageGrid(fig, 111, nrows_ncols=(rows, cols), axes_pad=0)

    for i in range(rows*cols):
        grid[i].imshow(images[i])
        grid[i].axis('off')
    plt.show(block=True)	

def plot_cost(costs_train,costs_validation):
	epochs_arr = np.arange(0, N_EPOCHS).tolist()

	plt.plot(epochs_arr, costs_train, 'r-',label='training loss')
	plt.plot(epochs_arr, costs_validation, 'b-',label='validation loss')
	plt.legend(loc='upper center', shadow=True)
	plt.xlabel('Epoch')
	plt.ylabel('Loss')
	plt.show()


#train_svm()


costs_validation,costs_train,W,b = train_softmax_reg()
plot_cost(costs_train,costs_validation)