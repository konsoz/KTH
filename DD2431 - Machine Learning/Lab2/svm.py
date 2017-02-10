from cvxopt.solvers import qp
from cvxopt.base import matrix
import data as svm_data


import numpy, pylab, random, math

def linearKernel(x1,x2):
	ret = x1[0]*x2[0]+x1[1]*x2[1]+1
	#ret = matrix(x1).trans()*matrix(x2)+1
	return ret 

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

def optimize():
	P = getP(linearKernel, svm_data.data)
	#We create these just so that we can use qp fuction to minimize eq7
	N = len(svm_data.data)
	q = numpy.array([-1.0 for i in range(N)])
	h = numpy.array([0.0 for i in range(N)])
	temp = numpy.array(q)
	G = numpy.diag(temp)
	
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

	pylab.show()

result = optimize()
decision(linearKernel,result)