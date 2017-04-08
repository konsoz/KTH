import numpy as np
import tools as tools
#from sklearn.mixture import log_multivariate_normal_density
import matplotlib.pyplot as plt

def gmm_recognition(models,tidigits):
    for tidigit in tidigits:
        model_log_likelihoods = []  
        for m in xrange(len(models)):
            model = models[m]
            gmm_model = model.get('gmm')
            log_emlik_gmm = log_emlik(tidigit.get('mfcc'),gmm_model.get('means'),gmm_model.get('covars'))
            log_lik = gmmloglik(log_emlik_gmm,gmm_model.get('weights'))
            model_log_likelihoods.append(log_lik)
        print("Tidigit :", tidigit.get('digit'))
        print("Has highest log liklehood with following model: ")
        best_model_id = model_log_likelihoods.index(max(model_log_likelihoods))
        best_model = models[best_model_id]
        print("Model was for digit %s", best_model.get('digit'))


def log_emlik(X, means, covars):
    return tools.log_multivariate_normal_density_diag(X,means,covars)

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
    result = 0
    for i in range(0,log_emlik.shape[0]):
        obsloglik = log_emlik[i]
        log_weights = np.log(weights)
        logsumexp = tools.logsumexp(log_weights+obsloglik)
        result += logsumexp
      
    return result

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

    gmm_recognition(models,tidigits)

    #transition = hmm_model.get('transmat')
    #startprob = hmm_model.get('startprob')

    #log_emlik_hmm = log_emlik(example.get('mfcc'),hmm_model.get('means'),hmm_model.get('covars'))

    #print(transition)
    #print(startprob)
    
    #plt.pcolormesh(np.rot90(log_emlik_hmm,k=4).T)
    #plt.show()


if __name__ == "__main__":
    main()



"""

Notes:

Multivariate Gaussian Density

y - state, x - time

Given state 0 (silence), the probability of time interval where we actually speak 
to state 0 is lower since we are speaking. Same thing applies to the last state.


"""