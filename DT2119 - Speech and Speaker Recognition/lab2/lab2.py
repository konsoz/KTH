import numpy as np
import tools as tools
#from sklearn.mixture import log_multivariate_normal_density
import matplotlib.pyplot as plt

def gmm_with_hmm_recognition(models,tidigits):
    wrong = 0
    for tidigit in tidigits:
        
        model_log_likelihoods = []
        
        for m in xrange(len(models)):
            model = models[m]
            hmm_model = model.get('hmm')
            gmm_model = model.get('gmm')
            
            log_emlik_gmm = log_emlik(tidigit.get('mfcc'),hmm_model.get('means'),hmm_model.get('covars'))
            log_lik = gmmloglik(log_emlik_gmm,gmm_model.get('weights'))
            model_log_likelihoods.append(log_lik)
        
        #print("Tidigit :", tidigit.get('digit'))
        #print("Has highest log liklehood with following model: ")
        best_model_id = model_log_likelihoods.index(max(model_log_likelihoods))
        best_model = models[best_model_id]
        #print("Model was for digit %s", best_model.get('digit'))
        
        if(tidigit.get('digit') != best_model.get('digit')):
            wrong += 1
    print "GMM with HMM means/covars has wrong", wrong, "predictions" 

def hmm_recognition(models,tidigits):
    wrong = 0
    for tidigit in tidigits:
        
        model_log_likelihoods = []
        
        for m in xrange(len(models)):
            model = models[m]
            hmm_model = model.get('hmm')
            transition = hmm_model.get('transmat')
            startprob = hmm_model.get('startprob')
            
            log_emlik_hmm = log_emlik(tidigit.get('mfcc'),hmm_model.get('means'),hmm_model.get('covars'))
            _, log_lik = forward(log_emlik_hmm,np.log(startprob),np.log(transition))
            model_log_likelihoods.append(log_lik)
        
        #print("Tidigit :", tidigit.get('digit'))
        #print("Has highest log liklehood with following model: ")
        best_model_id = model_log_likelihoods.index(max(model_log_likelihoods))
        best_model = models[best_model_id]
        #print("Model was for digit %s", best_model.get('digit'))
        
        if(tidigit.get('digit') != best_model.get('digit')):
            wrong += 1
    print "HMM has wrong", wrong, "predictions" 

def gmm_recognition(models,tidigits):
    wrong = 0
    for tidigit in tidigits:
        model_log_likelihoods = []
        for m in xrange(len(models)):
            model = models[m]
            gmm_model = model.get('gmm')
            log_emlik_gmm = log_emlik(tidigit.get('mfcc'),gmm_model.get('means'),gmm_model.get('covars'))
            log_lik = gmmloglik(log_emlik_gmm,gmm_model.get('weights'))
            model_log_likelihoods.append(log_lik)
        #print("Tidigit :", tidigit.get('digit'))
        #print("Has highest log liklehood with following model: ")
        best_model_id = model_log_likelihoods.index(max(model_log_likelihoods))
        best_model = models[best_model_id]
        #print("Model was for digit %s", best_model.get('digit'))

        if(tidigit.get('digit') != best_model.get('digit')):
            wrong += 1
    print "GMM has wrong", wrong, "predictions" 

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
    
    log_alpha = np.zeros_like(log_emlik)

    """Forward probabilities in log domain.

    Args:
        log_emlik: NxM array of emission log likelihoods, N frames, M states
        log_startprob: log probability to start in state i
        log_transmat: log transition probability from state i to j

    Output:
        forward_prob: NxM array of forward log probabilities for each of the M states in the model
    """

    # First log alpha
    log_alpha[0] = log_startprob + log_emlik[0]

    for n in range(1,log_alpha.shape[0]):
        for j in range(0,log_alpha.shape[1]):
            log_emlik_j_n = log_emlik[n,j]
            log_alpha_prev = log_alpha[n-1]
            log_transmat_j = log_transmat[:,j]
            log_alpha[n,j] = tools.logsumexp(log_alpha_prev+log_transmat_j) + log_emlik_j_n 

    log_alpha_last = log_alpha[log_alpha.shape[0]-1]
    sequence_likelihood = tools.logsumexp(log_alpha_last)

    return log_alpha, sequence_likelihood 

    #print(log_emlik.shape)
    #print(log_startprob.shape)
    #print(log_transmat.shape)

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
    log_viterbi = np.zeros_like(log_emlik)
    best_path = np.zeros_like(log_emlik)

    log_viterbi[0] = log_startprob + log_emlik[0]

    for n in range(1,log_viterbi.shape[0]):
        for j in range(0,log_viterbi.shape[1]):
            
            log_emlik_j_n = log_emlik[n,j]
            log_viterbi_prev = log_viterbi[n-1]
            log_transmat_j = log_transmat[:,j]

            # Viterbi approximation
            s = log_viterbi_prev+log_transmat_j
            log_viterbi[n,j] = np.max(s) + log_emlik_j_n
            
            # Best path
            #print(np.argmax(s))
            best_path[n,j] = np.argmax(s) 

    log_viterbi_last = log_viterbi[log_viterbi.shape[0]-1]
    log_lik = tools.logsumexp(log_viterbi_last)
    
    ## Check this code, result is wrong
    best_path = np.delete(best_path,0,0)

    s_star = np.zeros(best_path.shape[0])
    for i in range(0,best_path.shape[0]):
        s_star[i] = np.argmax(best_path[i])
    
    # Backtracking
    best = np.zeros(best_path.shape[0])

    for i in range(best_path.shape[0]-1,0,-1):
        index = int(s_star[i])
        best[i] = best_path[i,index]

    print(best)
    return log_lik


def main():
    example = np.load('lab2_example.npz')['example'].item()
    models = np.load('lab2_models.npz')['models']
    tidigits = np.load('lab2_tidigits.npz')['tidigits']



    #hmm_recognition(models,tidigits)
    #gmm_recognition(models,tidigits)
    #gmm_with_hmm_recognition(models,tidigits)
    

    
    hmm_model = models[0]['hmm']
    log_emlik_hmm = log_emlik(example.get('mfcc'),hmm_model.get('means'),hmm_model.get('covars'))
    transition = hmm_model.get('transmat')
    startprob = hmm_model.get('startprob')

    #log_alpha, sequence_likelihood = forward(log_emlik_hmm,np.log(startprob),np.log(transition))
    
    vlog_lik = viterbi(log_emlik_hmm,np.log(startprob),np.log(transition))
    
    

    #plt.imshow(log_alpha.T)
    #plt.show()
    
    #print(np.array_equal(log_alpha,correct_log_alpha))
    #gmm_model = models[0]['gmm']

    #gmm_recognition(models,tidigits)



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