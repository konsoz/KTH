import numpy, random, pylab

#numpy.random.seed(100)

classA = [(random.normalvariate(-1.5,1), random.normalvariate(0.5,0.5), 1.0) for i in range(5)] + \
		 [(random.normalvariate(1.5,1), random.normalvariate(0.5,0.5),1.0) for i in range(5)]

classB = [(random.normalvariate(0.0,0.5),random.normalvariate(-1.5,0.5),-1.0) for i in range(10)]

data = classA + classB
random.shuffle(data)

#pylab.show()
#print(data)
