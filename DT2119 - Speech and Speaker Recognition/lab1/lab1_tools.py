# DT2118, Lab 1 Feature Extraction
# Functions to be implemented ----------------------------------
#from __future__ import division
import numpy as np
import math as math
import scipy.signal as scipysignal
import matplotlib.pyplot as plt
from scipy.fftpack import fft, fftshift
from scipy.fftpack.realtransforms import dct

EXAMPLE = np.load('example.npz')['example'].item()

def enframe(samples, winlen, winshift,samplingrate):
    """
    Slices the input samples into overlapping windows.
    
    Args:
        winlen: window length in samples.
        winshift: shift of consecutive windows in samples
    Returns:
        numpy array [N x winlen], where N is the number of windows that fit
        in the input signal
    """

    sample_len = (1/float(samplingrate))*1000
    sample_step = winlen/sample_len
    sample_shift_step = winshift/sample_len
    number_of_windows = math.ceil(((samples.size/sample_step)*2)-2)

    sample_step = int(sample_step)

    frames = np.zeros((int(number_of_windows),sample_step))

    j = 2
    for i in range(0,int(number_of_windows)): 
        frames[i]= samples[i*200:sample_step*j/2] 
        j += 1     

    #print(samples.shape)
    
    graph = np.zeros((samples.size,2))

    j = 0
    for i in range(0,samples.size):
        graph[i][0] = j
        graph[i][1] = samples[i]
        j += 0.05

    #plt.imshow(test,origin='lower',interpolation='nearest',aspect='auto')
    #plt.plot(test[0:samples.size,0],test[0:samples.size,1])
    #plt.show() 

    return frames

def filter(frame,B,A):
        return scipysignal.lfilter(B,A,frame)

def preemp(frames, p=0.97):
    """
    Pre-emphasis filter.

    Args:
        input: array of speech frames [N x M] where N is the number of frames and
               M the samples per frame
        p: preemhasis factor (defaults to the value specified in the exercise)

    Output:
        output: array of pre-emphasised speech samples
    Note (you can use the function lfilter from scipy.signal)
    """
    
    B = [1,-p]
    A = 1
    emph = np.zeros((frames.shape[0],frames.shape[1]))

    for i in range(0,frames.shape[0]):
        emph[i] = filter(frames[i],B,A)

    #plt.imshow(emph,origin='lower',interpolation='nearest',aspect='auto')
    #plt.show()


    return emph

def windowing(preemph):
    """
    Applies hamming window to the input frames.

    Args:
        input: array of speech samples [N x M] where N is the number of frames and
               M the samples per frame
    Output:
        array of windoed speech samples [N x M]
    Note (you can use the function hamming from scipy.signal, include the sym=0 option
    if you want to get the same results as in the example)
    """

    hamming_window = scipysignal.hamming(400,sym=0)
    windowed = np.zeros((preemph.shape[0],preemph.shape[1]))

    for i in range(0,preemph.shape[0]):
        windowed[i] = preemph[i]*hamming_window

    #print(np.array_equal(windowed,EXAMPLE.get('windowed')))

    #plt.imshow(preemph,origin='lower',interpolation='nearest',aspect='auto')
    #plt.show()

    """
    plt.figure()
    A = fft(hamming_window, 2048) / (len(hamming_window)/2.0)
    freq = np.linspace(-0.5, 0.5, len(A))
    response = 20 * np.log10(np.abs(fftshift(A / abs(A).max())))
    plt.plot(freq, response)
    plt.axis([-0.5, 0.5, -120, 0])
    plt.title("Frequency response of the Hamming window")
    plt.ylabel("Normalized magnitude [dB]")
    plt.xlabel("Normalized frequency [cycles per sample]")

    plt.show()
    """
    return windowed

def powerSpectrum(windowed, nfft):
    """
    Calculates the power spectrum of the input signal, that is the square of the modulus of the FFT

    Args:
        input: array of speech samples [N x M] where N is the number of frames and
               M the samples per frame
        nfft: length of the FFT
    Output:
        array of power spectra [N x nfft]
    Note: you can use the function fft from scipy.fftpack
    """

    fast_fuorier_transformed = np.absolute(fft(windowed,nfft))**2

    #plt.imshow(fast_fuorier_transformed,origin='lower',interpolation='nearest',aspect='auto')
    #plt.show()

    return fast_fuorier_transformed    


def logMelSpectrum(spectrum, samplingrate,trfbank):
    """
    Calculates the log output of a Mel filterbank when the input is the power spectrum

    Args:
        input: array of power spectrum coefficients [N x nfft] where N is the number of frames and
               nfft the length of each spectrum
        samplingrate: sampling rate of the original signal (used to calculate the filterbanks)
    Output:
        array of Mel filterbank log outputs [N x nmelfilters] where nmelfilters is the number
        of filters in the filterbank
    Note: use the trfbank function provided in tools.py to calculate the filterbank shapes and
          nmelfilters
    """
    mel_filter_bank = trfbank(samplingrate,spectrum.shape[1])
    
    log_mel_spectrum = np.zeros((spectrum.shape[0],mel_filter_bank.shape[0]))

    for i in range(0,spectrum.shape[0]):
        log_mel_spectrum[i] = (np.matrix(spectrum[i])*mel_filter_bank.T)
        
    log_mel_spectrum = np.log(log_mel_spectrum)

    return log_mel_spectrum

    #plt.imshow(log_mel_spectrum,origin='lower',interpolation='nearest',aspect='auto')
    #plt.show()


def cepstrum(mspec, nceps):
    """
    Calulates Cepstral coefficients from mel spectrum applying Discrete Cosine Transform

    Args:
        input: array of log outputs of Mel scale filterbank [N x nmelfilters] where N is the
               number of frames and nmelfilters the length of the filterbank
        nceps: number of output cepstral coefficients
    Output:
        array of Cepstral coefficients [N x nceps]
    Note: you can use the function dct from scipy.fftpack.realtransforms
    """
    coefficients = dct(mspec,norm='ortho')
    return coefficients[:,:nceps]

def ecludian_distances(vec):
    return np.linalg.norm(vec)

def euclideans(one_mfcc,other_mfcc):
    ecludians = np.zeros((one_mfcc.shape[0],other_mfcc.shape[0]))

    for i in range(0,one_mfcc.shape[0]):
        for j in range(0,other_mfcc.shape[0]):
            ecludians[i,j] = ecludian_distances(one_mfcc[i]-other_mfcc[j])

    return ecludians

def dtw(localdist):
    """Dynamic Time Warping.

    Args:
        localdist: array NxM of local distances computed between two sequences
                   of length N and M respectively

    Output:
        globaldist: scalar, global distance computed by Dynamic Time Warping
    """
    N = localdist.shape[0] 
    M = localdist.shape[1]

    Dtw = np.zeros((N,M))

    for i in range(1,N):
        for j in range(1,M):
            loc_d = localdist[i][j]
            Dtw[i,j] = loc_d + min(Dtw[i-1,j],Dtw[i,j-1],Dtw[i-1,j-1])

    globaldist = Dtw[i,j]
    
    return globaldist 


def global_distances(all_mfcc):
    num_uttrances = len(all_mfcc)
    D = np.zeros((num_uttrances,num_uttrances))
    
    for i in range(0,num_uttrances):
        for j in range(0,num_uttrances):
            localdist = euclideans(all_mfcc[i],all_mfcc[j])
            globaldist = dtw(localdist)
            D[i,j] = globaldist
    
    return D