from __future__ import print_function
import itertools
import numpy as np
import tflearn
import gzip
import pickle
from tflearn.data_utils import to_categorical
from tflearn.data_preprocessing import DataPreprocessing
import matplotlib.pyplot as plt
from scipy.interpolate import spline
from sklearn.metrics import confusion_matrix
from utils import GENRE_LIST


class MonitorCallback(tflearn.callbacks.Callback):
    def __init__(self):
        self.accuracy_validation = []
        self.accuracy_train_temp = []
        self.accuracy_train = []

    def on_epoch_end(self, training_state):
    	self.accuracy_validation.append(training_state.val_acc)
    	self.accuracy_train.append(training_state.acc_value)

# Open dataset
with gzip.open('train.pkl.gz','rb') as f:
	data, labels = pickle.load(f)

with gzip.open('valid.pkl.gz','rb') as f:
	valid_data, valid_labels = pickle.load(f)

with gzip.open('test.pkl.gz','rb') as f:
	test_data, test_labels = pickle.load(f)

labels_to_indexes = {
	0 : 0,
	1 : 1,
	6 : 2,
	3 : 3
}

number_hidden = 256

num_labels = 10

data = np.vstack(data)

#labels = [ labels_to_indexes[l] for l in labels]
labels = np.asarray(labels)
labels = to_categorical(labels, num_labels)


valid_data = np.vstack(valid_data)
#valid_labels = [ labels_to_indexes[l] for l in valid_labels]
valid_labels = np.asarray(valid_labels)
valid_labels = to_categorical(valid_labels, num_labels)

test_data = np.vstack(test_data)
#test_labels = [ labels_to_indexes[l] for l in test_labels]
test_labels = np.asarray(test_labels)

number_features = data.shape[1]
number_examples = data.shape[0]

data_prep = DataPreprocessing()
data_prep.add_featurewise_zero_center()
#data_prep.add_featurewise_stdnorm()


# Build neural network
net = tflearn.input_data(shape=[None, number_features], data_preprocessing=data_prep)

# 1 fully connected
net = tflearn.fully_connected(net, number_hidden)
#tflearn.add_weights_regularizer(net, loss='L2')
net = tflearn.batch_normalization(net)
net = tflearn.relu(net)

# 2

net = tflearn.fully_connected(net, number_hidden)
#tflearn.add_weights_regularizer(net, loss='L2')
net = tflearn.relu(net)

# 3

net = tflearn.fully_connected(net, number_hidden)
#tflearn.add_weights_regularizer(net, loss='L2')
net = tflearn.batch_normalization(net)
net = tflearn.relu(net)

# 4

net = tflearn.fully_connected(net, number_hidden)
#tflearn.add_weights_regularizer(net, loss='L2')
net = tflearn.batch_normalization(net)
net = tflearn.relu(net)

net = tflearn.dropout(net,0.5)
net = tflearn.fully_connected(net, num_labels, activation='softmax')
net = tflearn.regression(net, loss='categorical_crossentropy', optimizer='momentum', learning_rate=0.001)

monitor_cb = MonitorCallback()

n_epoch = 30

# Define model
model = tflearn.DNN(
	net, 
	tensorboard_verbose=0, 
	tensorboard_dir='/home/konstantin/Documents/KTH-git/DT2119 - Speech and Speaker Recognition/project/logs')
# Start training (apply gradient descent algorithm)


model.load('models/4hidden_256_allgenres.tfl')
predicted = model.predict(test_data)

predicted_labels = np.zeros_like(test_labels)

for i,p in enumerate(predicted):
	predicted_labels[i] = np.argmax(p)

def plot_confusion_matrix(cm, classes,
                          normalize=False,
                          title='',
                          cmap=plt.cm.Blues):
    """
    This function prints and plots the confusion matrix.
    Normalization can be applied by setting `normalize=True`.
    """
	plt.imshow(cm, interpolation='nearest', cmap=cmap)
    plt.title(title)
    plt.colorbar()
    tick_marks = np.arange(len(classes))
    plt.xticks(tick_marks, classes, rotation=45)
    plt.yticks(tick_marks, classes)

    if normalize:
        cm = cm.astype('float') / cm.sum(axis=1)[:, np.newaxis]
        print("Normalized confusion matrix")
    else:
        print('Confusion matrix, without normalization')

    print(cm)

    thresh = cm.max() / 2.
    for i, j in itertools.product(range(cm.shape[0]), range(cm.shape[1])):
        plt.text(j, i, cm[i, j],
                 horizontalalignment="center",
                 color="white" if cm[i, j] > thresh else "black")

    plt.tight_layout()
    plt.ylabel('True label')
    plt.xlabel('Predicted label')


# Compute confusion matrix
cnf_matrix = confusion_matrix(test_labels, predicted_labels)
np.set_printoptions(precision=2)

# Plot non-normalized confusion matrix
plt.figure()
plot_confusion_matrix(cnf_matrix, classes=GENRE_LIST)


plt.show()

"""
model.fit(
	data, 
	labels, 
	n_epoch=n_epoch,
	batch_size=128,
	shuffle=True,
	show_metric=True,
	validation_set=(valid_data, valid_labels),
	run_id='4hidden_256_allgenres',
	callbacks=monitor_cb
	)


predicted = model.predict(test_data)



n_correct = 0
for i in xrange(test_labels.shape[0]):
	if np.argmax(predicted[i]) == test_labels[i]:
		n_correct += 1


print(float(n_correct)/test_labels.shape[0])



def plot_cost(accuracy_validation,accuracy_train):
	epochs_arr = np.arange(0, n_epoch)

	#x_smooth = np.linspace(epochs_arr.min(), epochs_arr.max(), 100)
	#y_smooth = spline(epochs_arr, accuracy_validation, x_smooth)

	plt.plot(epochs_arr, accuracy_train, 'r-',label='Train data')
	plt.plot(epochs_arr, accuracy_validation, 'b-',label='Validation data')
	plt.yticks(np.arange(0, 1.1, 0.1))
	plt.legend(loc=1, shadow=True)
	plt.xlabel('Batch')
	plt.grid(True)
	plt.ylabel('Error rate')
	plt.show()


err_valid = [ 1 - acc for acc in monitor_cb.accuracy_validation]
err_train = [ 1 - acc for acc in monitor_cb.accuracy_train]


plot_cost(err_valid, err_train)


model.save('4hidden_256_allgenres.tfl')

"""