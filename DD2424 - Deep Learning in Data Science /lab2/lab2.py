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

class NeuralNetwork:

	NODES_IN_HIDDEN = 50

	def __init__(self,data_dim, num_labels):
		self.W1 = np.random.normal(0,0.01,(self.NODES_IN_HIDDEN, data_dim))
		self.W1 = np.random.normal(0,0.01,(num_labels, data_dim))

		self.b1 = np.zeros((self.NODES_IN_HIDDEN,1))
		self.b1 = np.zeros((num_labels,1))


		# Note from stanford course:
		# It turns out that we can normalize the variance of each neuron's output to 
		# 1 by scaling its weight vector by the square root of its fan-in (i.e. its number of inputs). 
		# That is, the recommended heuristic is to initialize each neuron's weight vector as: 
		# w = np.random.randn(n) / sqrt(n), where n is the number of its inputs. 


def main():
	data = DataLoader()
	network = NeuralNetwork(data.X_train.shape[0],data.Y_train.shape[0])


if __name__ == "__main__":
    main()