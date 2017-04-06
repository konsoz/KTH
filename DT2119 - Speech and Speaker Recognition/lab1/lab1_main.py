import numpy as np
import lab1_tools as tools
import matplotlib.pyplot as plt
from scipy.cluster.hierarchy import linkage,dendrogram
from sklearn import mixture
from matplotlib.colors import LogNorm
import itertools
from scipy import linalg

color_iter = itertools.cycle(['navy', 'c', 'cornflowerblue', 'gold',
                              'darkorange'])
# DT2118, Lab 1 Feature Extraction
# - Functions given by the exercise -------------------------------------------- 

def mfcc(samples, winlen = 400, winshift = 200, nfft=512, nceps=13, samplingrate=20000, liftercoeff=22):
    """Computes Mel Frequency Cepstrum Coefficients.

    Args:
        samples: array of speech samples with shape (N,)
        winlen: lenght of the analysis window
        winshift: number of samples to shift the analysis window at every time step
        nfft: length of the Fast Fourier Transform (power of 2, >= winlen)
        nceps: number of cepstrum coefficients to compute
        samplingrate: sampling rate of the original signal
        liftercoeff: liftering coefficient used to equalise scale of MFCCs

    Returns:
        N x nceps array with lifetered MFCC coefficients
    """

    frames = tools.enframe(samples, 20, 10,samplingrate)
    preemph = tools.preemp(frames, 0.97)
    windowed = tools.windowing(preemph)
    spec = tools.powerSpectrum(windowed, nfft)
    mspec = tools.logMelSpectrum(spec, samplingrate,trfbank)
    ceps = tools.cepstrum(mspec, nceps)
    return lifter(ceps, liftercoeff)

def tidigit2labels(tidigitsarray):
    """
    Return a list of labels including gender, speaker, digit and repetition information for each
    utterance in tidigitsarray. Useful for plots.
    """
    labels = []
    nex = len(tidigitsarray)
    for ex in range(nex):
        labels.append(tidigitsarray[ex]['gender'] + '_' + 
                      tidigitsarray[ex]['speaker'] + '_' + 
                      tidigitsarray[ex]['digit'] + '_' + 
                      tidigitsarray[ex]['repetition'])
    return labels

def dither(samples, level=1.0):
    """
    Applies dithering to the samples. Adds Gaussian noise to the samples to avoid numerical
        errors in the subsequent FFT calculations.

        samples: array of speech samples
        level: decides the amount of dithering (see code for details)

    Returns:
        array of dithered samples (same shape as samples)
    """
    return samples + level*np.random.normal(0,1, samples.shape)
    

def lifter(mfcc, lifter=22):
    """
    Applies liftering to improve the relative range of MFCC coefficients.

       mfcc: NxM matrix where N is the number of frames and M the number of MFCC coefficients
       lifter: lifering coefficient

    Returns:
       NxM array with lifeterd coefficients
    """
    nframes, nceps = mfcc.shape
    cepwin = 1.0 + lifter/2.0 * np.sin(np.pi * np.arange(nceps) / lifter)
    return np.multiply(mfcc, np.tile(cepwin, nframes).reshape((nframes,nceps)))

def hz2mel(f):
    """Convert an array of frequency in Hz into mel."""
    return 1127.01048 * np.log(f/700 +1)

def trfbank(fs, nfft, lowfreq=133.33, linsc=200/3., logsc=1.0711703, nlinfilt=13, nlogfilt=27, equalareas=False):
    """Compute triangular filterbank for MFCC computation.

    Inputs:
    fs:         sampling frequency (rate)
    nfft:       length of the fft
    lowfreq:    frequency of the lowest filter
    linsc:      scale for the linear filters
    logsc:      scale for the logaritmic filters
    nlinfilt:   number of linear filters
    nlogfilt:   number of log filters

    Outputs:
    res:  array with shape [N, nfft], with filter amplitudes for each column.
            (N=nlinfilt+nlogfilt)
    From scikits.talkbox"""
    # Total number of filters
    nfilt = nlinfilt + nlogfilt

    #------------------------
    # Compute the filter bank
    #------------------------
    # Compute start/middle/end points of the triangular filters in spectral
    # domain
    freqs = np.zeros(nfilt+2)
    freqs[:nlinfilt] = lowfreq + np.arange(nlinfilt) * linsc
    freqs[nlinfilt:] = freqs[nlinfilt-1] * logsc ** np.arange(1, nlogfilt + 3)
    if equalareas:
        heights = np.ones(nfilt)
    else:
        heights = 2./(freqs[2:] - freqs[0:-2])

    # Compute filterbank coeff (in fft domain, in bins)
    fbank = np.zeros((nfilt, nfft))
    # FFT bins (in Hz)
    nfreqs = np.arange(nfft) / (1. * nfft) * fs
    for i in range(nfilt):
        low = freqs[i]
        cen = freqs[i+1]
        hi = freqs[i+2]

        lid = np.arange(np.floor(low * nfft / fs) + 1,
                        np.floor(cen * nfft / fs) + 1, dtype=np.int)
        lslope = heights[i] / (cen - low)
        rid = np.arange(np.floor(cen * nfft / fs) + 1,
                        np.floor(hi * nfft / fs) + 1, dtype=np.int)
        rslope = heights[i] / (hi - cen)
        fbank[i][lid] = lslope * (nfreqs[lid] - low)
        fbank[i][rid] = rslope * (hi - nfreqs[rid])

    return fbank


def feature_correlation():
    tidigits = np.load('tidigits.npz')['tidigits']
    all_mfcc = []

    for i in range(0,tidigits.size):
        sample = tidigits[i].get('samples')
        one_mfcc = mfcc(sample)
        all_mfcc.append(one_mfcc)

    Xtrain = np.matrix(all_mfcc[0])

    for i in range(1,len(all_mfcc)):
        Xtrain = np.concatenate((Xtrain,all_mfcc[i]))

    correlation_coefficients = np.zeros((Xtrain.shape[1],Xtrain.shape[1]))

    for i in range(0, Xtrain.shape[1]):
        for j in range(0, Xtrain.shape[1]):
            coeff = np.array(Xtrain[:,i]).T.reshape(-1)
            another_coeff = np.array(Xtrain[:,j]).T.reshape(-1)
            correlation_coefficients[i,j] = np.correlate(coeff,another_coeff)

    corr_coef = np.corrcoef(Xtrain, rowvar=0)

    plt.pcolormesh(corr_coef)
    plt.show()

def compare_uttrances():
    tidigits = np.load('tidigits.npz')['tidigits']
    all_mfcc = []

    for i in range(0,tidigits.size):
        sample = tidigits[i].get('samples')
        one_mfcc = mfcc(sample)
        all_mfcc.append(one_mfcc)

    D = tools.global_distances(all_mfcc)

    labels = tidigit2labels(tidigits)
    Z = linkage(D,method='complete')

    dendrogram(Z,labels=labels)
    plt.show()

def gaussian_mixture():
    tidigits = np.load('tidigits.npz')['tidigits']
    all_mfcc = []

    for i in range(0,tidigits.size):
        sample = tidigits[i].get('samples')
        one_mfcc = mfcc(sample)
        all_mfcc.append(one_mfcc)

    Xtrain = np.matrix(all_mfcc[0])

    for i in range(1,len(all_mfcc)):
        Xtrain = np.concatenate((Xtrain,all_mfcc[i]))

    gmm = mixture.GaussianMixture(n_components=32).fit(Xtrain)

    mfcc_seven = []

    mfcc_seven.append(mfcc(tidigits[16].get('samples')))
    mfcc_seven.append(mfcc(tidigits[17].get('samples')))
    mfcc_seven.append(mfcc(tidigits[38].get('samples')))
    mfcc_seven.append(mfcc(tidigits[39].get('samples')))

    Xpred_only_seven = np.matrix(mfcc_seven[0])

    for i in range(1,len(mfcc_seven)):
        Xpred_only_seven = np.concatenate((Xpred_only_seven,mfcc_seven[i]))


    predicted = gmm.predict_proba(Xpred_only_seven)

    plt.plot(predicted)
    plt.show()
   




feature_correlation()
