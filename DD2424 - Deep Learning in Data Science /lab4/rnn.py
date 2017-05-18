import numpy as np


class DataLoader:

	def __init__(self, file_name):
		# Input data and unique charachters
		self.book = open(file_name, 'r').read()
		self.uniq_chars = list(set(self.book))

		# Size of the book and vocabulary size
		self.book_size = len(self.book)
		self.vocab_size = len(self.uniq_chars)

		# Mappings from char to ind and vice versa
		self.char_to_ix = { ch:i for i,ch in enumerate(self.uniq_chars) }
		self.ix_to_char = { i:ch for i,ch in enumerate(self.uniq_chars) }



class RNN:

	def __init__(self, hidden_dim, learning_rate, input_size, output_size):
		# Length of input and output sequences, same for this exercise
		self.input_size = input_size
		self.output_size = output_size

		# Learning rate, eta
		self.learning_rate = learning_rate

		# Model parameters

		# hidden state at time t of size m x 1
		self.h = np.zeros((hidden_dim, 1))

		# weight matrix of size m x m applied to ht-1 (hidden-to-hidden connection)
		self.W = np.random.randn(hidden_dim, hidden_dim) * 0.01
		# weight matrix of size m x d applied to xt (input-to-hidden connection)
		self.U = np.random.randn(hidden_dim, self.input_size) * 0.01
		# weight matrix of size C x m applied to at (hidden-to-output connection)
		self.V = np.random.randn(self.output_size, hidden_dim) * 0.01

		# bias vector of size m x 1 in equation for at
		self.b = np.zeros((hidden_dim, 1))
		# bias vector of size C x 1 in equation for ot
		self.c = np.zeros((self.output_size, 1))

		# Adagrad params

		self.ada_W = np.zeros((hidden_dim, hidden_dim))
		self.ada_U = np.zeros((hidden_dim, self.input_size))
		self.ada_V = np.zeros((self.output_size, hidden_dim))
		self.ada_b = np.zeros((hidden_dim, 1))
		self.ada_c = np.zeros((self.output_size, 1))

	def forward(self, h, x, y):
		p = {}
		seq_length = len(x)
		loss = 0

		for t in range(seq_length):
			# One hot x
			x_t = np.zeros((self.input_size, 1))
			x_t[x[t]] = 1

			# find new hidden state
			a_t = np.dot(self.U, x_t) + np.dot(self.W, h[t-1]) + self.b
			h[t] = np.tanh(a_t)

			# unnormalized log probabilities for next chars o_t
			o_t = np.dot(self.V, h[t]) + self.c

			# Softmax
			p[t] = np.exp(o_t) / np.sum(np.exp(o_t))

			# cross-entropy loss
			loss += -np.log(p[t][y[t], 0])

		return loss, p

	def backward(self, x, y, p, h):
		# derivatives w.r.t different model params
		dU = np.zeros_like(self.U)
		dW = np.zeros_like(self.W)
		dV = np.zeros_like(self.V)
		db = np.zeros_like(self.b)
		dc = np.zeros_like(self.c)
		dh_next = np.zeros_like(self.h)

		for t in reversed(range(len(x))):
			# One hot y
			y_t = np.zeros((self.input_size, 1))
			y_t[y[t]] = 1

			# gradient w.r.t. o_t
			g = - (y_t - p[t])

			# gradient w.r.t. V and c
			dW += np.dot(g, h[t].T)
			dc += g

			# gradient w.r.t.


		pass

	def train(self, x, y):
		# Access the previous state to calculate the current state
		h = {}
		h[-1] = np.copy(self.h)




		# Forward pass
		loss, p = self.forward(h, x, y)
		self.backward(x, y, p, h)


		return loss


def main():
	data = DataLoader('goblet_book.txt')
	input_size = len(data.uniq_chars)
	output_size = len(data.uniq_chars)
	np.random.seed(400)
	rnn = RNN(100, 0.1, input_size, output_size)

	seq_length = 25
	losses = []
	smooth_loss = -np.log(1.0/len(data.uniq_chars))*seq_length
	losses.append(smooth_loss)

	for i in range(data.book_size/seq_length):
		x = [data.char_to_ix[c] for c in data.book[i*seq_length:(i+1)*seq_length]]#inputs to the RNN
		y = [data.char_to_ix[c] for c in data.book[i*seq_length+1:(i+1)*seq_length+1]]#the targets it should be outputting

		#if i%1000==0:
		#	sample_ix = rnn.sample(x[0], 200)
		#	txt = ''.join([ix_to_char[n] for n in sample_ix])
		#	print txt

		loss = rnn.train(x, y)
		smooth_loss = smooth_loss*0.999 + loss*0.001

		if i%1000==0:
			print 'iteration %d, smooth_loss = %f' % (i, smooth_loss)
			losses.append(smooth_loss)


if __name__ == "__main__":
	main()