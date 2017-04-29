import numpy as np
import cPickle
from random import randrange
import matplotlib.pyplot as plt
import math

class DataLoader:

	TRAIN_SIZE = 10000
	VALIDATION_SIZE = 1000
	NUM_BATCH_FILES = 5
	NUM_LABELS = 10
	N_BATCH = 100

	def __init__(self):
		X, Y, y = self.load_train_and_validation()
		
		self.X_train = X[:,0:self.TRAIN_SIZE]
		self.y_train = y[0:self.TRAIN_SIZE]
		self.Y_train = Y[:,0:self.TRAIN_SIZE]

		self.X_valid = X[:, self.TRAIN_SIZE : self.TRAIN_SIZE + self.VALIDATION_SIZE]  
		self.y_valid = y[self.TRAIN_SIZE : self.TRAIN_SIZE + self.VALIDATION_SIZE]  
		self.Y_valid = Y[:, self.TRAIN_SIZE : self.TRAIN_SIZE + self.VALIDATION_SIZE]  

		self.X_test, self.y_test, self.Y_test = self.load_test()

		self.preprocess()

		self.Xbatches, self.Ybatches = self.get_mini_batches(self.X_train,self.Y_train)

	def preprocess(self):
		X_train_mean = np.mean(self.X_train, axis = 1)
		self.X_train -= X_train_mean
		self.X_valid -= X_train_mean
		self.X_test -= X_train_mean

	def unpickle(self,file):
		fo = open(file, 'rb')
		dict = cPickle.load(fo)
		fo.close()
		return dict

	def load_train_and_validation(self):
		data = [self.unpickle('data_batch_'+ str(i)) for i in range(1,self.NUM_BATCH_FILES+1)]
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
	
		Y = np.zeros((self.NUM_LABELS,X.shape[1]))
		Y = ((np.arange(y.max()+1) == y[:,:]).astype(int)).T

		return X,Y,y

	def load_test(self):
		test_data = self.unpickle('test_batch')
		Xtest = np.matrix(test_data.get('data')).T
		Xtest = Xtest.astype('float64') / 255
		y_test = np.array(test_data.get('labels')).reshape(-1,1)
		Ytest = ((np.arange(y_test.max()+1) == y_test[:,:]).astype(int)).T
		return Xtest,y_test, Ytest

	def get_mini_batches(self,Xtrain,Ytrain):
		Xbatches = []
		Ybatches = []
		for j in range(1,(Xtrain.shape[1]/self.N_BATCH)+1):
			j_start = (j-1)*self.N_BATCH
			j_end = j*self.N_BATCH
			Xbatch = Xtrain[:,j_start:j_end]
			Ybatch = Ytrain[:,j_start:j_end]
			Xbatches.append(Xbatch)
			Ybatches.append(Ybatch)
		return Xbatches,Ybatches

class NeuralNetwork:

	N_EPOCHS = 10
	
	# Hyper-parameters
	MU = 0.9
	DECAY_RATE = 0.95

	def __init__(self,data_dim, num_labels, init_mode, num_layers, eta=0.1, lmbd=0):

		# Hyper-parameters
		self.LAMBDA = lmbd
		self.ETA = eta

		self.num_layers = num_layers
		
		self.bn_params = {}

		# Data dim
		# How should I not hard code it since number of hidden nodes vary?...
		self.biases_dims = []
		self.biases_dims.append((50,1))
		self.biases_dims.append((30,1))
		#self.biases_dims.append((50,1))
		self.biases_dims.append((num_labels,1))

		self.weights_dims = []
		self.weights_dims.append((50,data_dim))
		self.weights_dims.append((30,50))
		#self.weights_dims.append((50,50))
		self.weights_dims.append((num_labels,30))

		self.biases = []
		self.weights = []
		self.biases_moment = []
		self.weights_moment = []

		# Hidden biases m x 1 and momentum
		for i in xrange(num_layers):
			dim = self.biases_dims[i]
			self.biases.append(np.zeros(dim))
			self.biases_moment.append(np.zeros(dim))
	
		# Hidden weights m x m and momentum
		for i in xrange(num_layers):
			dim = self.weights_dims[i]
			self.weights.append(np.random.normal(0,0.01,dim))
			self.weights_moment.append(np.zeros(dim))


	def score_function(self, X, W, b):
		return W*X+b

	def relu_activation(self,scores):
		return np.maximum(0, scores)

	def softmax_probabilities(self,scores):
		return (np.exp(scores) / np.sum(np.exp(scores), axis=0))

	def predict(self,X):
		P, _, _ = self.forward_pass(X)
		preds = np.argmax(P,axis=0).T
		return preds

	def compute_cost(self,X,Y):
		P, _, _ , _= self.forward_pass(X)
		cross_entropy = 0
		for i in range(0,X.shape[1]):
			y = Y[:,i]
			p = P[:,i]
			ent = -np.log(y.T*p)
			cross_entropy += ent
		num_imgs = X.shape[1]
		cost = (1/float(num_imgs))* cross_entropy
		for weight in self.weights:
			cost += self.LAMBDA*np.sum(weight*weight)
		return cost[0,0]


	def compute_gradients(self, X, Y, P, dot_products, hidden_layers):
		num_imgs = X.shape[1]

		grads_w = []
		grads_b = []

		dscores = (P - Y) / num_imgs
		hidden_layers.insert(0,X)
		i = self.num_layers - 1

		while i >= 0:
			# Gradient with respect to bias
			# Some strange with numpy, sometimes I get an (N,) instead of (N,1)...
			if i == self.num_layers - 1:
				d_b = dscores.sum(axis=1)
			else:
				d_b = np.matrix(dscores.sum(axis=1)).T

			# Gradeint with respect to weight
			d_w = (dscores * hidden_layers[i].T)
			d_w += 2 * self.LAMBDA * self.weights[i]
			
			grads_w.append(d_w)
			grads_b.append(d_b)
			
			# Ugly backpropagation into hidden layer and relu 
			# How to do this in matrix form?
			if i > 0:
				all_g = np.zeros((self.weights[i].shape[1],num_imgs)) 
				for im in xrange(num_imgs):
					g = np.matrix(dscores[:, [im]].T) * self.weights[i]
					s = dot_products[i-1][:,im].T
					s[s <= 0] = 0
					s[s > 0] = 1
					s = np.diagflat(s) 
					g = g*s
					all_g[:,im] = g
				dscores = all_g
			
			i -= 1

		return grads_w[::-1], grads_b[::-1]

	def forward_pass(self, X, mode='train'):
		dot_products = []
		hidden_layers = []
		dot_products_bn = []
		means = []
		sigmas = []

		
		alpha = 0.99

		for layer in range(0, self.num_layers-1):
			if layer == 0:
				s = self.score_function(X, self.weights[layer], self.biases[layer])
				dot_products.append(s)
					
				if mode == "train":
					# Calculate s_hat, means and vars
					s_hat, mu, var = self.bn_forward(s)
					means.append(mu)
					sigmas.append(var)

					# Update running mean and var
				if mode == "test":
					pass

				dot_products_bn.append(s_hat)
				hidden_layers.append(self.relu_activation(s_hat))
			else:
				s = self.score_function(hidden_layers[layer - 1], self.weights[layer], self.biases[layer])
				dot_products.append(s)
					
				if mode == "train":
					# Calculate s_hat, means and vars
					s_hat, mu, var = self.bn_forward(s)
					means.append(mu)
					sigmas.append(var)

					# Update running mean and var
				if mode == "test":
					pass

				dot_products_bn.append(s_hat)
				hidden_layers.append(self.relu_activation(s_hat))

		# Final layer and softmax
		scores_final = self.score_function(hidden_layers[-1], self.weights[-1], self.biases[-1])
		dot_products.append(scores_final)
		probs = self.softmax_probabilities(scores_final)

		return probs, dot_products, dot_products_bn, hidden_layers

	def bn_forward(self, s, running_mean=None, running_var=None):
		eps = 1e-5
	
		# Mean and variance
		mu = 1 / float(s.shape[1]) * np.sum(s,axis=1)
		xmu = s - mu
		carre = np.power(xmu,2)
		var = 1 / float(s.shape[1]) * np.sum(carre, axis=1)
		
		# Normalized scores - train
		if running_mean is None and running_var is None:
			sqrtvar = np.sqrt(var + eps)
			invvar = (1. / sqrtvar)
			diag = np.diagflat(invvar)
			s_hat = diag * xmu
		# Normalized scores test
		else: 
			sqrtvar = np.sqrt(running_var + eps)
			invvar = 1. / sqrtvar
			diag = np.diagflat(invvar)
			xmu = s - running_mean
			s_hat = diag * xmu

		return s_hat, mu, var


	def train(self,Xbatches,Ybatches,X_train,Y_train, X_valid, Y_valid):
	
		for epoch in xrange(self.N_EPOCHS):
			for batch in xrange(len(Xbatches)):
				## Forward pass ##
				
				probs, dot_products, dot_products_bn, hidden_layers = self.forward_pass(Xbatches[batch])

				# Compute gradients and perform backprop
				grads_w, grads_b = self.compute_gradients(Xbatches[batch], Ybatches[batch], probs, dot_products, hidden_layers)
				
				## Gradient check 
				grad_check_sparse(lambda _: self.compute_cost(Xbatches[batch], Ybatches[batch]), self.weights[0], grads_w[0], 1)

				# Momentum update
				
				for i in xrange(len(self.weights)):
					self.weights_moment[i] = self.MU * self.weights_moment[i] + self.ETA * grads_w[i]
					self.weights[i] -= self.weights_moment[i]

					self.biases_moment[i] = self.MU * self.biases_moment[i] + self.ETA * grads_b[i]
					self.biases[i] -= self.biases_moment[i]
				
			# Eta update for momentum
			self.ETA = self.ETA * self.DECAY_RATE
			#train_cost = self.compute_cost(X_train, Y_train)
			validation_cost = self.compute_cost(X_train, Y_train)
			#print "Cost train:", train_cost
			print "Cost validation:", validation_cost
			#print "Epoch", epoch, "done.."


def grad_check_sparse(f, x, analytic_grad, num_checks):
  """
  Adapted from: http://cs231n.github.io/neural-networks-case-study/ and http://cs231n.github.io/neural-networks-3/#gradcheck
  sample a few random elements and only return numerical
  in this dimensions.
  """
  h = 1e-5

  x.shape
  for i in xrange(num_checks):
    ix = tuple([randrange(m) for m in x.shape])

    oldval = x[ix]
    x[ix] = oldval + h # increment by h
    fxph = f(x) # evaluate f(x + h)
    #print type(fxph), type(fxmh)
    x[ix] = oldval - h # increment by h
    fxmh = f(x) # evaluate f(x - h)
    x[ix] = oldval # reset

    grad_numerical = (fxph - fxmh) / (2 * h)
    grad_analytic = analytic_grad[ix]
    rel_error = abs(grad_numerical - grad_analytic) / (abs(grad_numerical) + abs(grad_analytic))
    # Sometimes rel_error becomes NaN when analytic and numerical gradient are 0
    if not math.isnan(rel_error):
    	print 'numerical: %f analytic: %f, relative error: %e' % (grad_numerical, grad_analytic, rel_error)


def plot_cost(costs_train,costs_validation):
	epochs_arr = np.arange(0, 10).tolist()

	plt.plot(epochs_arr, costs_train, 'r-',label='training loss')
	plt.plot(epochs_arr, costs_validation, 'b-',label='validation loss')
	plt.legend(loc='upper center', shadow=True)
	plt.xlabel('Epoch')
	plt.ylabel('Loss')
	plt.show()

def get_accuracy(networks_predictions, y):

	preds = np.zeros(y.shape)
	ensamble = np.concatenate(networks_predictions,axis=1)

	for i in xrange(preds.shape[0]):
		v = ensamble[i].tolist()[0]
		votes = np.bincount(v)
		winning_vote = np.argmax(votes)
		preds[i] = winning_vote

	diff = preds - y
	num_correct = diff[diff == 0].shape[0]
	num_tot = y.shape[0]
	acc = num_correct / float(num_tot)
	return acc*100

def main():
	data = DataLoader()

	# Found during fine search with momentum update
	eta = 0.030764
	lmbd = 0
	
	# Ensemble networks with different initializations
	n_networks = 1
	networks_predictions = []

	for i in xrange(n_networks):
		network = NeuralNetwork(data.X_train.shape[0], data.Y_train.shape[0], i, 3, eta, lmbd)
		network.train(data.Xbatches, data.Ybatches, data.X_train, data.Y_train, data.X_valid, data.Y_valid)
		preds = network.predict(data.X_test)
		networks_predictions.append(preds)

	test_accuracy = get_accuracy(networks_predictions, data.y_test)
	print 'Test accuracy: %f || Eta: %f || Lambda: %f' % (test_accuracy,eta,lmbd)

if __name__ == "__main__":
    main()
