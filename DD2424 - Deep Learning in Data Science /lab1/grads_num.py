import numpy as np
import lab1 as lab

def compute_grads_num(X, Y, W, b, lbda, h):
    # Numerical check for gradients
    no = W.shape[0]
    d = X.shape[1]

    grad_W = np.zeros(W.shape)
    grad_b = np.zeros(no)

    P = lab.evaluate_classifier(X,W,b)
    c = lab.compute_cost(X, Y, W,P)

    for i in range(no):
        b_try = np.copy(b)
        b_try[i] += h
        P = lab.evaluate_classifier(X,W,b)
        c2 = lab.compute_cost(X, Y, W, P)
        grad_b[i] = (c2 - c) / h

    for i in range(W.shape[0]):
        for j in range(W.shape[1]):
            W_try = np.copy(W)
            W_try[i, j] = W_try[i, j] + h;
            P = lab.evaluate_classifier(X,W,b)
            c2 = lab.compute_cost(X, Y, W_try, P)
            
            grad_W[i, j] = (c2 - c) / h

    return grad_W, grad_b
