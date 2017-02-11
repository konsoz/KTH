import numpy, random, pylab

numpy.random.seed(100)

classA = [(random.normalvariate(-1.5,0.7), random.normalvariate(1.5,0.7), 1.0) for i in range(10)] + \
		 [(random.normalvariate(1.5,0.7), random.normalvariate(0.5,0.7),1.0) for i in range(10)]

classB = [(random.normalvariate(0.0,0.5),random.normalvariate(-0.5,0.5),-1.0) for i in range(20)]

data = classA + classB
random.shuffle(data)

#pylab.show()
#print(data)
