import os
import glob
import sys
import numpy as np
import scipy
import scipy.io.wavfile
#from python_speech_features import mfcc
from scikits.talkbox.features import mfcc

import cPickle, gzip
from utils import TRAIN_DIR, GENRE_LIST


genre_labels = {
    "blues" : 0,
    "classical" : 1,
    "country" : 2,
    "disco" : 3,
    "hiphop": 4,
    "jazz": 5,
    "metal": 6,
    "pop": 7,
    "reggae" : 8,
    "rock": 9
}


features = []
labels = [] 

def dump():
    print "Dumping..."
    with gzip.open('test.pkl.gz', 'wb') as f:
        cPickle.dump((features, labels), f)

def create_ceps(fn, genre):
    """
        Creates the MFCC features. 
    """    
    sample_rate, X = scipy.io.wavfile.read(fn)
    X[X==0]=1
    ceps, mspec, spec = mfcc(X, fs=sample_rate)
    label = genre_labels[genre]

    n_frames = 4133
    n_coeff = 13
    n_song_parts = 4
    n_frames_from_each_slice = 138

    # Ignore 2 features with other dims..
    if ceps.shape == (n_frames,n_coeff):
            for i in xrange(n_song_parts):  
                part_of_song = int(n_frames / n_song_parts)
                next_part_of_song = part_of_song * (i+1)
                slice_of_part = ceps[i*part_of_song:next_part_of_song,:]
                s = slice_of_part[0:n_frames_from_each_slice,:]
                features.append(s)
                [ labels.append(label) for _ in xrange(n_frames_from_each_slice)]


if __name__ == "__main__":
    import timeit
    start = timeit.default_timer()
    
    for subdir, dirs, files in os.walk(TRAIN_DIR):
        traverse = list(set(dirs).intersection( set(GENRE_LIST) ))
        break
    
    print "Working with these genres --> ", traverse
    print "Starting ceps generation"     
    for subdir, dirs, files in os.walk(TRAIN_DIR):
        for file in files:
            path = subdir+'/'+file
            if path.endswith("wav"):
                tmp = subdir[subdir.rfind('/',0)+1:]
                if tmp in traverse:
                    create_ceps(path, tmp)
    

    dump()

    stop = timeit.default_timer()
    print "Total ceps generation and feature writing time (s) = ", (stop - start) 