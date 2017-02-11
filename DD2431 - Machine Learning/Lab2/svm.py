from cvxopt.solvers import qp
from cvxopt.base import matrix
from cvxopt import solvers
import data as svm_data
import numpy, pylab, random, math

solvers.options['maxiters'] = 1000

SIGMA = 1
C = 0.5

def linearKernel(x1,x2):
	result = x1[0]*x2[0]+x1[1]*x2[1]+1
	return result 

def rbsKernel(x1,x2):
	euclidean = (x1[0]-x2[0])**2+(x1[1]-x2[1])**2
	return numpy.exp(-(euclidean/(2*SIGMA**2)))

def getP(func, data):
	N = len(data)
	P = [ [ data[i][2]*data[j][2]*func(data[i], data[j]) for i in range(N)] for j in range(N)]
	return P

# x star is the new point
def indicator(kernel_func, x_alpha_pair, new_point):
	result = 0
	for i in range(len(x_alpha_pair)):
		alpha = x_alpha_pair[i][1]
		x = x_alpha_pair[i][0][:2]
		t = x_alpha_pair[i][0][2]
		result += alpha*t*kernel_func(new_point,x)
	return result

def optimize(kernel_func):
	P = getP(kernel_func, svm_data.data)
	#We create these just so that we can use qp fuction to minimize eq7
	N = len(svm_data.data)
	q = numpy.array([-1.0 for i in range(N)])
	h = numpy.array([0.0 for i in range(N)]+[C for i in range(N)])
	temp = numpy.array(q)
	G1 = numpy.diag(temp)
	G2 = numpy.diag(-temp)
	G = numpy.vstack((G1,G2))
	
	#WE USE THE qp
	r = qp(matrix(P), matrix(q), matrix(G), matrix(h))
	alpha = list(r['x'])
	x_alpha_pair = []
	
	# create alpha x pair
	for i in range(N):
		if(alpha[i] > 10**(-5)):
			x_alpha_pair.append((svm_data.data[i], alpha[i]))

	return x_alpha_pair

def decision(kernel_func,x_alpha_pair):
	xrange = numpy.arange(-4,4,0.05)
	yrange = numpy.arange(-4,4,0.05)

	grid = matrix([[indicator(kernel_func, x_alpha_pair, (x,y)) for y in yrange] for x in xrange])

	pylab.hold(True)

	pylab.plot([p[0] for p in svm_data.classA], [p[1] for p in svm_data.classA],'bo')
	pylab.plot([p[0] for p in svm_data.classB], [p[1] for p in svm_data.classB],'ro')

	pylab.contour(xrange,yrange,grid,(-1.0,0.0,1.0), colors=('red','black','blue'),linewidths=(1,3,1))
	pylab.xlabel('X1')
	pylab.ylabel('X2')
	pylab.title('Decision Boundary with Sigma = ' + str(SIGMA) + ' and C = '+ str(C))
	pylab.show()

result = optimize(rbsKernel)
decision(rbsKernel,result)


"""
Question about sigma

Large sigma squared value, features vary more smoothly - higher bias, lower variance
Small sigma squared value, features vary adruptly - low bias, high variance

Questions about slack variables

1. For small values of C we penalize (straffa) less the points within the marigins. (We accept more points inside the margin)
For bigger values of C we penalize more the points within the marings. (We accept less point inside the margin)

2. Large C (small margin) = Low bias, high variance
   Small C (wide margin) = High bias, low variance

   We should use more slack when we know that tha data contains some noise, 
"""