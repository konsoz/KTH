import numpy as np
import cPickle
from random import randrange
import matplotlib.pyplot as plt
import math

class DataLoader:

	TRAIN_SIZE = 49000
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

	NODES_IN_HIDDEN = 100
	N_EPOCHS = 30
	
	# Hyper-parameters
	MU = 0.9
	DECAY_RATE = 0.95
	# Smoothing term
	EPS = 1e-8

	# Adam beta
	BETA1 = 0.9
	BETA2 = 0.999

	def __init__(self,data_dim, num_labels, init_mode, eta=0.1, lmbd=0):

		# Hyper-parameters
		self.LAMBDA = lmbd
		self.ETA = eta

		# Bias
		self.b1 = np.zeros((self.NODES_IN_HIDDEN,1))
		self.b2 = np.zeros((num_labels,1))

		if init_mode == 1:
			self.W1 = np.random.normal(0,0.01,(self.NODES_IN_HIDDEN, data_dim))
			self.W2 = np.random.normal(0,0.01,(num_labels, self.NODES_IN_HIDDEN))

		if init_mode == 2:
			self.W1 = np.zeros((self.NODES_IN_HIDDEN,data_dim))
			for i in xrange(self.W1.shape[0]):
				self.W1[i] = np.random.randn(self.W1.shape[1]) * math.sqrt(2.0/self.W1.shape[1])

			self.W2 = np.zeros((num_labels, self.NODES_IN_HIDDEN))
			for i in xrange(self.W2.shape[0]):
				self.W2[i] = np.random.randn(self.W2.shape[1]) * math.sqrt(2.0 / self.W2.shape[1])
		
		if init_mode == 3:
			self.W1 = np.zeros((self.NODES_IN_HIDDEN,data_dim))
			for i in xrange(self.W1.shape[0]):
				self.W1[i] = np.random.randn(self.W1.shape[1]) / math.sqrt(self.W1.shape[1])

			self.W2 = np.zeros((num_labels, self.NODES_IN_HIDDEN))
			for i in xrange(self.W2.shape[0]):
				self.W2[i] = np.random.randn(self.W2.shape[1]) / math.sqrt(self.W2.shape[1])

		if init_mode == 4:
			self.W1 = np.random.normal(0,0.1,(self.NODES_IN_HIDDEN, data_dim))
			self.W2 = np.random.normal(0,0.1,(num_labels, self.NODES_IN_HIDDEN))

		# Momentum
		self.W1_moment = np.zeros(self.W1.shape)
		self.W2_moment = np.zeros(self.W2.shape)
		self.b1_moment = np.zeros(self.b1.shape)
		self.b2_moment = np.zeros(self.b2.shape)

		"""
		# Cache for Adagrad / RMSprop
		self.W1_cache = np.zeros(self.W1.shape)
		self.W2_cache = np.zeros(self.W2.shape)
		self.b1_cache = np.zeros(self.b1.shape)
		self.b2_cache = np.zeros(self.b2.shape)

		# m,v for Adam

		self.W1_M = np.zeros(self.W1.shape)
		self.W1_V = np.zeros(self.W1.shape)

		self.W2_M = np.zeros(self.W2.shape)
		self.W2_V = np.zeros(self.W2.shape)

		self.b1_M = np.zeros(self.b1.shape)
		self.b1_V = np.zeros(self.b1.shape)

		self.b2_M = np.zeros(self.b2.shape)
		self.b2_V = np.zeros(self.b2.shape)
		"""

	def score_function(self, X, W, b):
		return W*X+b

	def relu_activation(self,scores):
		return np.maximum(0, scores)

	def softmax_probabilities(self,scores):
		return (np.exp(scores) / np.sum(np.exp(scores), axis=0))

	def predict(self,X):
		P = self.get_model_probs(X)
		preds = np.argmax(P,axis=0).T
		return preds

	def get_model_probs(self,X):
		scores_hidden = self.score_function(X,self.W1,self.b1)
		hidden_layer = self.relu_activation(scores_hidden)

		scores = self.score_function(hidden_layer,self.W2,self.b2)
		P = self.softmax_probabilities(scores)
		return P		
	
	def compute_cost(self,X,Y):
		"""
		scores_hidden = self.score_function(X,W1,b1)
		hidden_layer = self.relu_activation(scores_hidden)

		# TODO: convert to vectorized implementation
		scores = self.score_function(hidden_layer,W2,b2)
		P = self.softmax_probabilities(scores)
		"""
		P = self.get_model_probs(X)
		cross_entropy = 0
		for i in range(0,X.shape[1]):
			y = Y[:,i]
			p = P[:,i]
			ent = -np.log(y.T*p)
			cross_entropy += ent
		num_imgs = X.shape[1]
		cost = (1/float(num_imgs))* cross_entropy + self.LAMBDA*np.sum(self.W1*self.W1)+self.LAMBDA*np.sum(self.W2*self.W2)
		return cost


	def compute_gradients(self, X, Y, P, W1,W2,scores_hidden,hidden_layer):
		num_classes = Y.shape[0] 
		data_dim = X.shape[0] 
		num_imgs = X.shape[1]

		gradW2 = np.zeros((num_classes,self.NODES_IN_HIDDEN))
		gradb2 = np.zeros((num_classes,1))
		gradW1 = np.zeros((self.NODES_IN_HIDDEN,data_dim))
		gradb1 = np.zeros((self.NODES_IN_HIDDEN,1))

		for i in xrange(num_imgs):
			corresponding_softmax_probs = P[:,i]
			diag_p = np.diagflat(corresponding_softmax_probs) 
			g = -((Y[:,i] / (Y[:,i]*P[:,i])) * (diag_p-corresponding_softmax_probs*corresponding_softmax_probs.T))

			# Add gradient with respect to b2 and W2
			gradb2 += g.T
			gradW2 += g.T*hidden_layer[:,i].T

			# Propagate gradients
			g = g*W2
			
			s_1 = scores_hidden[:,i].T
			s_1[s_1 <= 0] = 0
			s_1[s_1 > 0] = 1
			s_1 = np.diagflat(s_1)

			g = g*s_1

			# Add gradient with respect to W1 and b1
			gradb1 += g.T
			gradW1 += g.T*X[:,i].T

		# Divide by number of entries in the mini-batch and add corresponding regularization term
		gradW2 /= num_imgs
		gradb2 /= num_imgs
		gradW2 = gradW2 + 2*self.LAMBDA*W2

		gradW1 /= num_imgs
		gradb1 /= num_imgs
		gradW1 = gradW1 + 2*self.LAMBDA*W1

		return gradW1,gradb1,gradW2,gradb2

	def train(self,Xbatches,Ybatches,X_train,Y_train,X_valid,Y_valid, y_valid):
		costs_train = []
		costs_validation = []
	
		for epoch in xrange(self.N_EPOCHS):
			for batch in xrange(len(Xbatches)):
				
				## Forward pass ##
				# evaluate class scores
				scores_hidden = self.score_function(Xbatches[batch],self.W1,self.b1)
				hidden_layer = self.relu_activation(scores_hidden)

				# Apply final linear transformation and compute the class probabilities
				scores = self.score_function(hidden_layer,self.W2,self.b2)
				probs = self.softmax_probabilities(scores)
				
				# Compute gradients and perform backprop
				gradW1, gradb1, gradW2, gradb2 = self.compute_gradients(Xbatches[batch],Ybatches[batch],
					probs,self.W1,self.W2,scores_hidden,hidden_layer)

				## Gradient check 
				#grad_check_sparse(lambda W: self.compute_cost(Xbatches[batch],Ybatches[batch],self.W1,self.W2,self.b1,self.b2), self.W2,gradW2,1)

				# Momentum update
				self.W1_moment = self.MU * self.W1_moment + self.ETA * gradW1
				self.W2_moment = self.MU * self.W2_moment + self.ETA * gradW2
				self.b1_moment = self.MU * self.b1_moment + self.ETA * gradb1
				self.b2_moment = self.MU * self.b2_moment + self.ETA * gradb2

				# Update weights and bias
				self.W1 -= self.W1_moment
				self.W2 -= self.W2_moment

				self.b1 -= self.b1_moment
				self.b2 -= self.b2_moment
				
				"""
				# Adam 
				self.W1_M = self.BETA1 * self.W1_M + (1 - self.BETA1) * gradW1
				self.W1_V = self.BETA2 * self.W1_V + (1 - self.BETA2) * (gradW1**2)
				self.W1 += - self.ETA * self.W1_M / (np.sqrt(self.W1_V) + self.EPS)

				self.W2_M = self.BETA1 * self.W2_M + (1 - self.BETA1) * gradW2
				self.W2_V = self.BETA2 * self.W2_V + (1 - self.BETA2) * (gradW2**2)
				self.W2 += - self.ETA * self.W2_M / (np.sqrt(self.W2_V) + self.EPS)

				self.b1_M = self.BETA1 * self.b1_M + (1 - self.BETA1) * gradb1
				self.b1_V = self.BETA2 * self.b1_V + (1 - self.BETA2) * (gradb1**2)
				self.b1 += - self.ETA * self.b1_M / (np.sqrt(self.b1_V) + self.EPS)

				self.b2_M = self.BETA1 * self.b2_M + (1 - self.BETA1) * gradb2
				self.b2_V = self.BETA2 * self.b2_V + (1 - self.BETA2) * (gradb2**2)
				self.b2 += - self.ETA * self.b2_M / (np.sqrt(self.b2_V) + self.EPS)

				# RMSprop
				self.W1_cache = self.DECAY_RATE * self.W1_cache + (1 - self.DECAY_RATE) * gradW1**2
				self.W2_cache = self.DECAY_RATE * self.W2_cache + (1 - self.DECAY_RATE) * gradW2**2
				self.b1_cache = self.DECAY_RATE * self.b1_cache + (1 - self.DECAY_RATE) * gradb1**2
				self.b2_cache = self.DECAY_RATE * self.b2_cache + (1 - self.DECAY_RATE) * gradb2**2

				self.W1 += - self.ETA * gradW1 / (np.sqrt(self.W1_cache) + self.EPS)
				self.W2 += - self.ETA * gradW2 / (np.sqrt(self.W2_cache) + self.EPS)
				self.b1 += - self.ETA * gradb1 / (np.sqrt(self.b1_cache) + self.EPS)
				self.b2 += - self.ETA * gradb2 / (np.sqrt(self.b2_cache) + self.EPS)

				# Adagrad
				self.W1_cache += gradW1**2
				self.W2_cache += gradW2**2
				self.b1_cache += gradb1**2
				self.b2_cache += gradb2**2

				self.W1 += - self.ETA * gradW1 / (np.sqrt(self.W1_cache) + self.EPS)
				self.W2 += - self.ETA * gradW2 / (np.sqrt(self.W2_cache) + self.EPS)
				self.b1 += - self.ETA * gradb1 / (np.sqrt(self.b1_cache) + self.EPS)
				self.b2 += - self.ETA * gradb2 / (np.sqrt(self.b2_cache) + self.EPS)

				"""
			# Eta update
			self.ETA = self.ETA * self.DECAY_RATE
			#cost_train = self.compute_cost(X_train, Y_train)
			#cost_validation = self.compute_cost(X_valid, Y_valid)
			#validation_acc = self.compute_acc(y_valid,X_valid)
			#print "Cost validation: %f" %cost_validation
			#print "Cost train: %f" %cost_train
			#print "Validation accuracy: " , validation_acc,  " %"
			#print "Epoch" , epoch, "done..."
			#costs_train.append(cost_train[0,0])
			#costs_validation.append(cost_validation[0,0])

		#plot_cost(costs_train,costs_validation)



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
    print 'numerical: %f analytic: %f, relative error: %e' % (grad_numerical, grad_analytic, rel_error)

def rel_error(x, y):
  """ returns relative error """
  return np.max(np.abs(x - y) / (np.maximum(1e-8, np.abs(x) + np.abs(y))))

def plot_cost(costs_train,costs_validation):
	epochs_arr = np.arange(0, 10).tolist()

	plt.plot(epochs_arr, costs_train, 'r-',label='training loss')
	plt.plot(epochs_arr, costs_validation, 'b-',label='validation loss')
	plt.legend(loc='upper center', shadow=True)
	plt.xlabel('Epoch')
	plt.ylabel('Loss')
	plt.show()

def get_accuracy(network1_pred, network2_pred, network3_pred, network4_pred, network5_pred, y):

	preds = np.zeros(y.shape)
	ensamble = np.concatenate((network1_pred,network2_pred,network3_pred,network4_pred,network5_pred),axis=1)

	for i in xrange(preds.shape[0]):
		v = ensamble[i].tolist()[0]
		votes = np.bincount(v)
		winning_vote = np.argmax(votes)
		preds[i] = winning_vote

	diff = preds - y
	print diff.shape
	num_correct = diff[diff == 0].shape[0]
	num_tot = y.shape[0]
	acc = num_correct / float(num_tot)
	return acc*100

def main():
	data = DataLoader()

	for i in range(1):

		eta = 0.030764
		lmbd = 0.002238
		
		network1 = NeuralNetwork(data.X_train.shape[0], data.Y_train.shape[0], 1, eta, lmbd)
		network1.train(data.Xbatches, data.Ybatches, data.X_train, data.Y_train, data.X_valid, data.Y_valid, data.y_valid)
		preds1 = network1.predict(data.X_test)

		network2 = NeuralNetwork(data.X_train.shape[0], data.Y_train.shape[0], 2, eta, lmbd)
		network2.train(data.Xbatches, data.Ybatches, data.X_train, data.Y_train, data.X_valid, data.Y_valid, data.y_valid)
		preds2 = network2.predict(data.X_test)

		network3 = NeuralNetwork(data.X_train.shape[0],data.Y_train.shape[0], 3, eta, lmbd)
		network3.train(data.Xbatches, data.Ybatches, data.X_train, data.Y_train, data.X_valid, data.Y_valid, data.y_valid)
		preds3 = network3.predict(data.X_test)

		network4 = NeuralNetwork(data.X_train.shape[0], data.Y_train.shape[0], 4, eta, lmbd)
		network1.train(data.Xbatches, data.Ybatches, data.X_train, data.Y_train, data.X_valid, data.Y_valid, data.y_valid)
		preds4 = network4.predict(data.X_test)

		network5 = NeuralNetwork(data.X_train.shape[0],data.Y_train.shape[0], 2, eta, lmbd)
		network5.train(data.Xbatches, data.Ybatches, data.X_train, data.Y_train, data.X_valid, data.Y_valid, data.y_valid)
		preds5 = network5.predict(data.X_test)

		test_accuracy = get_accuracy(preds1, preds2, preds3, preds4, preds5, data.y_test)
		print 'Test accuracy: %f || Eta: %f || Lambda: %f' % (test_accuracy,eta,lmbd)

if __name__ == "__main__":
    main()




"""
Notes :

Eta range : 0.3 - 0.002
Lambda range : 0.001 - 0.5 


"""