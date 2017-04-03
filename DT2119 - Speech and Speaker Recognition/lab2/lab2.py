import numpy as np
from tools import *
from sklearn.mixture import log_multivariate_normal_density

def log_emlik(X, means, covars):
    return log_multivariate_normal_density(X,means,covars)

def gmmloglik(log_emlik, weights):
    """Log Likelihood for a GMM model based on Multivariate Normal Distribution.

    Args:
        log_emlik: array like, shape (N, K).
            contains the log likelihoods for each of N observations and
            each of K distributions
        weights:   weight vector for the K components in the mixture

    Output:
        gmmloglik: scalar, log likelihood of data given the GMM model.
    """

def forward(log_emlik, log_startprob, log_transmat):
    """Forward probabilities in log domain.

    Args:
        log_emlik: NxM array of emission log likelihoods, N frames, M states
        log_startprob: log probability to start in state i
        log_transmat: log transition probability from state i to j

    Output:
        forward_prob: NxM array of forward log probabilities for each of the M states in the model
    """

def backward(log_emlik, log_startprob, log_transmat):
    """Backward probabilities in log domain.

    Args:
        log_emlik: NxM array of emission log likelihoods, N frames, M states
        log_startprob: log probability to start in state i
        log_transmat: transition log probability from state i to j

    Output:
        backward_prob: NxM array of backward log probabilities for each of the M states in the model
    """

def viterbi(log_emlik, log_startprob, log_transmat):
    """Viterbi path.

    Args:
        log_emlik: NxM array of emission log likelihoods, N frames, M states
        log_startprob: log probability to start in state i
        log_transmat: transition log probability from state i to j

    Output:
        viterbi_loglik: log likelihood of the best path
        viterbi_path: best path
    """


def main():
    example = np.load('lab2_example.npz')['example'].item()
    models = np.load('lab2_models.npz')['models']
    tidigits = np.load('lab2_tidigits.npz')['tidigits']

    hmm_model = models[0]['hmm']
    gmm_model = models[0]['gmm']

    log_emlik_hmm = log_emlik(example.get('mfcc'),hmm_model.get['means'],hmm_model.get('covars'))
    



if __name__ == "__main__":
    main()