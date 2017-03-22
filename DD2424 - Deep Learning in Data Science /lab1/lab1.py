import numpy as np
import cPickle

NUM_LABELS = 10

def unpickle(file):
    fo = open(file, 'rb')
    dict = cPickle.load(fo)
    fo.close()
    return dict


def load_batch():
	data = unpickle('data_batch_1')
	X = np.matrix(data.get('data')).T
	Y = np.zeros((NUM_LABELS,X.shape[1]))
	y = np.array(data.get('labels')).reshape(-1,1)
	Y = ((np.arange(y.max()+1) == y[:,:]).astype(int)).T
	return [X,Y,y]




