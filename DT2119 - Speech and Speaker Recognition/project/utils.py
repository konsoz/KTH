import os
import sys
import timeit
import numpy as np
from pydub import AudioSegment

###################################################
#    Don't modify below this line
###################################################
config = {}
execfile("config.cfg", config)

TRAIN_DIR = config["TRAIN_DIR"]
TEST_DIR = config["TEST_DIR"]
GENRE_LIST = config["GENRE_LIST"]

if TRAIN_DIR is None or TRAIN_DIR is "":
    print "Please set TRAIN_DIR in config.cfg"
    sys.exit(1)

elif TEST_DIR is None or TEST_DIR is "":
    print "Please set TEST_DIR in config.cfg" 
    sys.exit(1)    

elif GENRE_LIST is None or len(GENRE_LIST)==0:
    print "Please set GENRE_LIST in config.cfg" 
    sys.exit(1)


def convert_dataset_to_wav():
    """
        Converts all files of the GTZAN dataset
        to the WAV (uncompressed) format.
    """
    start = timeit.default_timer()
    rootdir = TRAIN_DIR
    for subdir, dirs, files in os.walk(rootdir):
        for file in files:
            path = subdir+'/'+file
            if path.endswith("au"):
                song = AudioSegment.from_file(path,"au")
                song = song[:30000]
                song.export(path[:-3]+"wav",format='wav')

    for subdir, dirs, files in os.walk(rootdir):
        for file in files:
            path = subdir+'/'+file
            if not path.endswith("wav"):
                os.remove(path)

    stop = timeit.default_timer()
    print "Conversion time = ", (stop - start)

