import numpy as np
import cPickle
from random import randrange
import matplotlib.pyplot as plt
import math
import cv2
from scipy import ndimage

class DataLoader:

	TRAIN_SIZE = 49000
	VALIDATION_SIZE = 10000
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

		#self.augment_data()

		self.Xbatches, self.Ybatches = self.get_mini_batches(self.X_train,self.Y_train)

	def augment_data(self):
		with_noise = np.copy(self.X_train)
		with_noise_y = np.copy(self.y_train)
		with_noise_Y = np.copy(self.Y_train)

		rotated = np.copy(self.X_train[:, 0:self.TRAIN_SIZE])
		rotated_y = np.copy(self.y_train[0:self.TRAIN_SIZE])
		rotated_Y = np.copy(self.Y_train[:, 0:self.TRAIN_SIZE])

		for i in xrange(with_noise.shape[1]):
			with_noise[:, [i]] = self.blur(with_noise[:, [i]])
		
		for i in xrange(rotated.shape[1]):
			rotated[:, [i]] = self.rotate(rotated[:, [i]])

		self.X_train = np.concatenate((self.X_train, with_noise),axis=1)
		self.y_train = np.concatenate((self.y_train, with_noise_y),axis=0)
		self.Y_train = np.concatenate((self.Y_train, with_noise_Y),axis=1)


	def rotate(self, X):
		X = np.asarray(X)
		im = X.reshape(3,32,32)
		im = (im - im.min()) / (im.max() - im.min())
		im = im.T
		im = np.rot90(im,k=2)
		X = im.reshape(3072,1)
		return X

	def blur(self, X):
		return ndimage.gaussian_filter(X, sigma=1)

	def add_noise(self,X):
		# Select random channel to apply noise on
		
		rand = np.random.randint(0,3)
		noise = np.random.uniform(0.25,0.5, (1024, 1))
		zitter = np.zeros_like(X)

		if rand == 0:
			zitter[0:1024] = noise
		if rand == 1:
			zitter[1024:2048] = noise
		if rand == 2:
			zitter[2048:3072] = noise
		
		noise_added = cv2.add(X, zitter)
		
		return X

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

	N_EPOCHS = 8
	
	# Hyper-parameters
	MU = 0.9
	DECAY_RATE = 0.95

	EPS = 1e-5

	def __init__(self,data_dim, num_labels, init_mode, num_layers, eta=0.1, lmbd=0):

		# Hyper-parameters
		self.LAMBDA = lmbd
		self.ETA = eta

		self.num_layers = num_layers
		
		# Batch normalization stuff
		self.running_var = {}
		self.running_mean = {}

		# Layers dims
		self.biases_dims = []
		self.biases_dims.append((50,1))
		self.biases_dims.append((30,1))
		self.biases_dims.append((num_labels,1))

		self.weights_dims = []
		self.weights_dims.append((50,data_dim))
		self.weights_dims.append((30,50))
		self.weights_dims.append((num_labels,30))

		self.biases = []
		self.weights = []
		self.biases_moment = []
		self.weights_moment = []

		for i in xrange(num_layers):
			dim = self.biases_dims[i]
			self.biases.append(np.zeros(dim))
			self.biases_moment.append(np.zeros(dim))
	
		for i in xrange(num_layers):
			dim = self.weights_dims[i]
			
			if init_mode == 0:
				self.weights.append(np.random.normal(0,0.01,dim))

			if init_mode == 1:
				self.weights.append(np.random.normal(0,0.1,dim))

			if init_mode == 2:
				self.weights.append(np.random.normal(0,0.001,dim))
			
			self.weights_moment.append(np.zeros(dim))

	def score_function(self, X, W, b):
		return W*X+b

	def relu_activation(self,scores):
		return np.maximum(0, scores)

	def softmax_probabilities(self,scores):
		return (np.exp(scores) / np.sum(np.exp(scores), axis=0))

	def predict(self,X):
		P, _, _ , _, _, _= self.forward_pass(X, 'test')
		preds = np.argmax(P,axis=0).T
		return preds

	def compute_cost(self,X,Y, mode):
		P, _, _ , _, _, _ = self.forward_pass(X, mode=mode)
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


	def compute_gradients(self, X, Y, P, dot_products, dot_products_bn, hidden_layers, means, sigmas):
		num_imgs = X.shape[1]

		grads_w = []
		grads_b = []

		# Insert input as first activation
		hidden_layers.insert(0, X)

		i = self.num_layers - 2

		# Last layer, without bn
		G = (P - Y) / num_imgs
		d_b = G.sum(axis=1)
		d_w = G * hidden_layers[-1].T
		d_w += 2 * self.LAMBDA * self.weights[-1]

		grads_w.append(d_w)
		grads_b.append(d_b)

		all_g = np.zeros((self.weights[-1].shape[1],num_imgs)) 
		for im in xrange(num_imgs):
			g = np.matrix(G[:, [im]].T) * self.weights[-1]
			s = dot_products_bn[-1][:,im].T
			s[s <= 0] = 0
			s[s > 0] = 1
			s = np.diagflat(s) 
			g = g*s
			all_g[:,im] = g
		G = all_g

		# Other layers in network, with bn
		while i >= 0:
			
			# Batch normalize
			G = self.bn_backward(G, dot_products[i], sigmas[i], means[i])
			# Gradient w.r.t bias
			d_b = np.matrix(G.sum(axis=1)).T
			# Gradient w.r.t weights
			d_w = (G * hidden_layers[i].T)
			d_w += 2 * self.LAMBDA * self.weights[i]
			
			grads_w.insert(0, d_w)
			grads_b.insert(0, d_b)
			
			# Backpropagate
			if i > 0:
				all_g = np.zeros((self.weights[i].shape[1],num_imgs)) 
				for im in xrange(num_imgs):
					g = np.matrix(G[:, [im]].T) * self.weights[i]
					s = dot_products_bn[i-1][:,im].T
					s[s <= 0] = 0
					s[s > 0] = 1
					s = np.diagflat(s) 
					g = g*s
					all_g[:,im] = g
				G = all_g
			
			i -= 1

		return grads_w, grads_b

	def bn_backward(self, g, s, s_var, s_mean):
		
		# Need ndarrays insted of matrix...fix this in later time
		h1 = np.asarray((s - s_mean).T)
		h2 = np.squeeze(np.asarray(1. / np.sqrt(s_var + self.EPS))) 
		sqrt_h2 = np.squeeze(np.asarray(np.sqrt(s_var + self.EPS)))
		squared_h1 = h1**2
		g = g.T
		s_var = np.asarray(s_var)
		
		N = g.shape[0]

		# Derivate h1
		dh1 = h2 * g
		# Derivate h1 inv
		dinv_h1 = np.sum(h1 * g, axis=0)
		# Derivate h2 sqrt
		dsqrt_h2 = -1. / (sqrt_h2**2) * dinv_h1
		# Derivate scores var
		dvar = 0.5 * (s_var.T + self.EPS)**(-0.5) * dsqrt_h2
		# Derivate squared h1
		dsquared_h1 = 1 / float(N) * np.ones((squared_h1.shape)) * dvar
		dh1 += 2 * h1 * dsquared_h1
		dx = dh1
		# Derivate scores mean
		dmu = - np.sum(dh1, axis=0)
		# Derivate scores
		dx += 1 / float(N) * np.ones((dh1.shape)) * dmu

		return dx.T


	def forward_pass(self, X, mode='train'):
		dot_products = []
		hidden_layers = []
		dot_products_bn = []
		means = []
		sigmas = []
		
		alpha = 0.99

		for layer in range(0, self.num_layers - 1):
			# For first layer, take X as input
			if layer == 0:
				# scores, un-normalized s
				s = self.score_function(X, self.weights[layer], self.biases[layer])
				dot_products.append(s)
			
				if mode == "train":
					# Calculate s_hat, means and vars
					s_hat, mu, var = self.bn_forward(s)
					
					means.append(mu)
					sigmas.append(var)

					# Update running mean and var
					# Get running mean and var from train or initialize default from first run
					running_mean = self.running_mean.get(layer, mu)
					running_var = self.running_var.get(layer, var)

					updated_mean = alpha * running_mean + (1 - alpha) * mu
					updated_var = alpha * running_var + (1 - alpha) * var

					self.running_mean[layer] = updated_mean
					self.running_var[layer] = updated_var
				
				if mode == "test":
					running_mean = self.running_mean.get(layer)
					running_var = self.running_var.get(layer)

					s_hat, _, _ = self.bn_forward(s, running_mean, running_var)
				
				# Activation, normalized s
				dot_products_bn.append(s_hat)
				hidden_layers.append(self.relu_activation(s_hat))
			# For other layers
			else:
				# scores, un-normalized s
				s = self.score_function(hidden_layers[layer - 1], self.weights[layer], self.biases[layer])
				dot_products.append(s)
					
				if mode == "train":
					# Calculate s_hat, means and vars
					s_hat, mu, var = self.bn_forward(s)
					means.append(mu)
					sigmas.append(var)

					# Update running mean and var

					running_mean = self.running_mean.get(layer, mu)
					running_var = self.running_var.get(layer, var)

					updated_mean = alpha * running_mean + (1 - alpha) * mu
					updated_var = alpha * running_var + (1 - alpha) * var

					self.running_mean[layer] = updated_mean
					self.running_var[layer] = updated_var
				
				if mode == "test":
					running_mean = self.running_mean.get(layer)
					running_var = self.running_var.get(layer)

					s_hat, _, _ = self.bn_forward(s, running_mean, running_var)
				
				# Activation, normalized s
				dot_products_bn.append(s_hat)
				hidden_layers.append(self.relu_activation(s_hat))

		# Final layer and softmax
		scores_final = self.score_function(hidden_layers[-1], self.weights[-1], self.biases[-1])
		probs = self.softmax_probabilities(scores_final)

		return probs, dot_products, dot_products_bn, hidden_layers, means, sigmas

	def bn_forward(self, s, running_mean=None, running_var=None):

		# Mean and variance
		s_mean = s.mean(axis=1)
		s_var = s.var(axis=1)

		# Normalized scores - train
		if running_mean is None and running_var is None:
			h1 = (s-s_mean)
			h2 = np.sqrt(s_var+self.EPS)
			s_hat = h1 / h2
		# Normalized scores test
		else:
			h1 = (s-running_mean)
			h2 = np.sqrt(running_var + self.EPS)
			s_hat = h1 / h2
			
		return s_hat, s_mean, s_var

	def train(self,Xbatches,Ybatches,X_train,Y_train, X_valid, Y_valid):
		costs_train = []
		costs_validation = []
		for epoch in xrange(self.N_EPOCHS):
			for batch in xrange(len(Xbatches)):
				## Forward pass ##
				
				probs, dot_products, dot_products_bn, hidden_layers, means, sigmas = self.forward_pass(Xbatches[batch])

				# Compute gradients and perform backprop
				grads_w, grads_b = self.compute_gradients(Xbatches[batch], Ybatches[batch], 
					probs, dot_products, dot_products_bn, hidden_layers, means, sigmas)
				
				## Gradient check 
				
				#grad_check_sparse(lambda _: self.compute_cost(Xbatches[batch], Ybatches[batch], 'train'), self.weights[0], grads_w[0], 1)

				# Momentum update
				
				for i in xrange(len(self.weights)):
					self.weights_moment[i] = self.MU * self.weights_moment[i] + self.ETA * grads_w[i]
					self.weights[i] -= self.weights_moment[i]

					self.biases_moment[i] = self.MU * self.biases_moment[i] + self.ETA * grads_b[i]
					self.biases[i] -= self.biases_moment[i]
				
			# Eta update for momentum
			self.ETA = self.ETA * self.DECAY_RATE
			cost_train = self.compute_cost(X_train, Y_train, 'test')
			cost_validation = self.compute_cost(X_valid, Y_valid, 'test')
			print "Cost validation: %f" %cost_validation
			print "Cost train: %f" %cost_train
			#costs_train.append(cost_train)
			#costs_validation.append(cost_validation)
			print "Epoch ", epoch, "done..."

		#plot_cost(costs_train,costs_validation, self.N_EPOCHS)


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


def plot_cost(costs_train,costs_validation, n_epochs):
	epochs_arr = np.arange(0, n_epochs).tolist()

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
	eta = 0.016
	lmbd = 0.00069
	
	# Ensemble networks with different initializations
	n_networks = 3
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
