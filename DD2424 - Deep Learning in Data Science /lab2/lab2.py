import numpy as np
import cPickle

class DataLoader:

	TRAIN_SIZE = 10000
	VALIDATION_SIZE = 10000
	NUM_BATCH_FILES = 2
	NUM_LABELS = 10

	def __init__(self):
		X, Y, y = self.load_train_and_validation()
		
		self.X_train = X[:,0:self.TRAIN_SIZE]
		self.y_train = y[0:self.TRAIN_SIZE]
		self.Y_train = Y[:,0:self.TRAIN_SIZE]

		self.X_valid = X[:,0:self.TRAIN_SIZE+self.VALIDATION_SIZE]  
		self.y_valid = y[0:self.TRAIN_SIZE+self.VALIDATION_SIZE]  
		self.Y_valid = Y[:,0:self.TRAIN_SIZE+self.VALIDATION_SIZE]  

		self.X_test, self.y_test, self.Y_test = self.load_test()

		self.preprocess()

		self.X_batches, self.Y_batches = self.get_mini_batches(self.X_train,self.Y_train)

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
		for j in range(1,(Xtrain.shape[1]/N_BATCH)+1):
			j_start = (j-1)*N_BATCH
			j_end = j*N_BATCH
			Xbatch = Xtrain[:,j_start:j_end]
			Ybatch = Ytrain[:,j_start:j_end]
			Xbatches.append(Xbatch)
			Ybatches.append(Ybatch)
		return Xbatches,Ybatches

class NeuralNetwork:

	NODES_IN_HIDDEN = 50
	LAMBDA = 0
	ETA = 0.01
	N_EPOCHS = 20

	def __init__(self,data_dim, num_labels):
		# Note from stanford course:
		# It turns out that we can normalize the variance of each neuron's output to 
		# 1 by scaling its weight vector by the square root of its fan-in (i.e. its number of inputs). 
		# That is, the recommended heuristic is to initialize each neuron's weight vector as: 
		# w = np.random.randn(n) / sqrt(n), where n is the number of its inputs.

		self.W1 = np.random.normal(0,0.01,(self.NODES_IN_HIDDEN, data_dim))
		self.W2 = np.random.normal(0,0.01,(num_labels, data_dim))

		self.b1 = np.zeros((self.NODES_IN_HIDDEN,1))
		self.b2 = np.zeros((num_labels,1))

	def score_function(self, X, W, b):
		return W*X+b

	def relu_activation(self,scores):
		return np.maximum(0, scores)

	def softmax_probabilities(self,scores):
		return (np.exp(scores) / np.sum(np.exp(scores), axis=0))

	def compute_cost(self,X,Y,P,W1,W2):
		# TODO: convert to vectorized implementation
		cross_entropy = 0
		for i in range(0,X.shape[1]):
			y = Y[:,i]
			p = P[:,i]
			ent = -np.log(y.T*p)
			cross_entropy += ent
		num_imgs = X.shape[1]
		cost = (1/float(num_imgs))* cross_entropy + self.LAMBDA*np.sum(W1*W1)+self.LAMBDA*np.sum(W2*W2)
		return cost

	def compute_gradients(X, Y, P, W1,W2,b,s_1):
		num_classes = Y.shape[0] 
		data_dim = X.shape[0] 
		num_imgs = X.shape[1]

		gradW2 = np.zeros((num_classes,data_dim))
		gradb2 = np.zeros((num_classes,1))
		gradW1 = np.zeros((self.NODES_IN_HIDDEN,data_dim))
		gradb1 = np.zeroos((self.NODES_IN_HIDDEN,1))

		for i in xrange(num_imgs):
			corresponding_softmax_probs = P[:,i]
			diag_p = np.diagflat(corresponding_softmax_probs) 
			g = -((Y[:,i] / (Y[:,i]*P[:,i])) * (diag_p-corresponding_softmax_probs*corresponding_softmax_probs.T))
			
			# Add gradient with respect to b2 and W2
			gradb2 += g.T
			gradW2 += g.T*X[:,i].T

			# Propagate gradients
			g = g*gradW2
			s_1[s_1 <= 0] = 0
			g = g*np.diagflat(s_1)

			# Add gradient with respect to W1 and b1
			gradb1 += g.T
			gradW1 += g.T*X[:,i].T

		# Divide by number of entries in the mini-batch and add corresponding regularization term
		gradW2 /= num_imgs
		gradb2 /= num_imgs
		gradW2 = gradW2 + 2*LAMBD*W2

		gradW1 /= num_imgs
		gradb1 /= num_imgs
		gradW1 = gradW1 + 2*LAMBD*W1

		return gradW,gradb

	def train(self,Xbatches,Ybatches,X_valid,Y_valid,y):
		costs_train = []
		costs_validation = []
	
		for epoch in xrange(self.N_EPOCHS):
			for batch in xrange(len(Xbatches)):
				
				## Forward pass ##
				# evaluate class scores		
				scores_hidden = self.score_function(Xbatches[batch],self.W1,self.b1)
				hidden_layer = self.relu_activation(scores_hidden)

				# Apply final linear transformation and compute the class probabilities
				scores = self.score_function(Xbatches[batch],self.W2,self.b2)

				gradW, gradB = compute_gradients(Xbatches[batch],Ybatches[batch],P,W,b)
				W = W - Eta*gradW
				b = b - Eta*gradB


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
		costs_train.append(cost_train[0,0])
		costs_validation.append(cost_validation[0,0])


def main():
	data = DataLoader()
	network = NeuralNetwork(data.X_train.shape[0],data.Y_train.shape[0])


if __name__ == "__main__":
    main()