import monkdata as mdata
import dtree as dtree
import drawtree_qt5 as draw
import random
import matplotlib.pyplot as plt
import numpy as numpy
## Assignment 0:
## 
## Our assumptions:
## 
## 1. MONK-2 the true concept behind this dataset is more "random" since any!! two attributes should be 1 
## it gives more margin for randomness for the other attributes. This leads to more uncertainty (higher entropy).
## this will perform bad because of its large uncertainty
## 
## 2. MONK-1 the true concept behind this dataset is more certain than MONK-2 because 
## the condition tells you what values specific!! attributes should have. 
## this will perform better than 2
##
## 3. MONK-3 has a more complex underlying concept with more specific information about each attribute, but it has additional 5% noise
## which is bad
## this will perform better than monk2 but worse than monk1 because of the 5% additional noise
## 
## Assignment 1:
## Calculate entropy of the training datasets
## Answer: (('entropy1', 1.0), ('entropy2', 0.957117428264771), ('entropy3', 0.9998061328047111))
## 
## Assignment 2:
## Entropy for an uniform distribution is high because the propability of outcomes is the same.  
## One example of an uniform distribution is (fair) coin toss because the propability of getting head or tail is the same. 
## 
## Entropy for an non-uniform distribution is low because the propability of outcomes is different and one outcome is more probable.
## One example of an non-uniform distribution is normal distribution where the probapility of all events is different.  
def ent():
	entropy_monk1 = dtree.entropy(mdata.monk1)
	entropy_monk2 = dtree.entropy(mdata.monk2)
	entropy_monk3 = dtree.entropy(mdata.monk3)
	return (('entropy1',entropy_monk1),('entropy2',entropy_monk2),('entropy3',entropy_monk3))


## Assignment 3: 
##('monk1-a-0 gain', 0.07527255560831925, 
##'monk2-a-0 gain', 0.0037561773775118823,
##'monk3-a-0 gain', 0.007120868396071844, 
##'monk1-a-1 gain', 0.005838429962909286, 
##'monk2-a-1 gain', 0.0024584986660830532,
##'monk3-a-1 gain', 0.29373617350838865, 
##'monk1-a-2 gain', 0.00470756661729721, 
##'monk2-a-2 gain', 0.0010561477158920196,
##'monk3-a-2 gain', 0.0008311140445336207,
##'monk1-a-3 gain', 0.02631169650768228, 
##'monk2-a-3 gain', 0.015664247292643818, 
##'monk3-a-3 gain', 0.002891817288654397, 
##'monk1-a-4 gain', 0.28703074971578435, 
##'monk2-a-4 gain', 0.01727717693791797,
## 'monk3-a-4 gain', 0.25591172461972755, 
## 'monk1-a-5 gain', 0.0007578557158638421
## 'monk2-a-5 gain', 0.006247622236881467,
## 'monk3-a-5 gain', 0.007077026074097326)
## 
## The attributes with highest gain should be choosen for splitting the examples at the root node (a5 for the MONK-1 dataset)
##
## Assignment 4: 
## The entropy of the subset is lower when the information gain is maximized. 
## When we choose proper attribute (with high information gain) we will reduce our uncernainty about the result decrease erntopy.
## 

def gain():
	monk1_gains = [];
	monk2_gains = [];
	monk3_gains = [];
	for i in range(0,6):
		monk1_gains.append((mdata.attributes[i],dtree.averageGain(mdata.monk1,mdata.attributes[i]))) 
		monk2_gains.append((mdata.attributes[i],dtree.averageGain(mdata.monk2,mdata.attributes[i])))
		monk3_gains.append((mdata.attributes[i],dtree.averageGain(mdata.monk3,mdata.attributes[i])))
	return [monk1_gains,monk2_gains,monk3_gains]


## Assignment 5:
## We learned that MONK-3 dataset is less sensitive to noise than we thought, this can be because the true concept has more underlying cases.
## It turned out that our assumptions about MONK-1 and MONK-2 was fairly right. 
## 
## Traing performance is as good as it can be because our model is overfitted to our training sample. 
## Since our model is overly specialized to the training set, it will perform much worse for the test data which was our case.
## This means it doesnt have a good generalization.
## The exception being MONK-3 which has a structured underlying pattern
## 
## MONK1 Performance on training set 1.0
## MONK1 Performance on test set 0.8287037037037037
## MONK2 Performance on training set 1.0
## MONK2 Performance on test set 0.6921296296296297
## MONK3 Performance on training set 1.0
## MONK3 Performance on test set 0.9444444444444444 
##
## Assignment 6: 
## A complex model (overfitting) leads to a high variance and low bias. 
## A less complex model (underfitting) leads to less variance and more bias.
## Pruning helps to delete some branches from the tree and make our model less complex, i.e. prevent from high variance. 
## 
##

def bldTree():
	tree_monk1 = dtree.buildTree(mdata.monk1,mdata.attributes)
	tree_monk2 = dtree.buildTree(mdata.monk2,mdata.attributes)
	tree_monk3 = dtree.buildTree(mdata.monk3,mdata.attributes)
	print('MONK1 Performance on training set',dtree.check(tree_monk1,mdata.monk1))
	print('MONK1 Performance on test set',dtree.check(tree_monk1,mdata.monk1test))
	print('MONK2 Performance on training set',dtree.check(tree_monk2,mdata.monk2))
	print('MONK2 Performance on test set',dtree.check(tree_monk2,mdata.monk2test))
	print('MONK3 Performance on training set',dtree.check(tree_monk3,mdata.monk3))
	print('MONK3 Performance on test set',dtree.check(tree_monk3,mdata.monk3test))
	##draw.drawTree(tree_monk1)

## Assignment 7:
##
##

def partition(data,fraction):
	ldata = list(data)
	random.shuffle(ldata)
	break_point = int(len(ldata)*fraction)
	return ldata[:break_point], ldata[break_point:]


def prune(tree, validation_set):
	best_tree = tree
	best_tree_performance = dtree.check(best_tree, validation_set)
	alternative_trees = dtree.allPruned(tree)

	for pruned_tree in alternative_trees:
		perf_pruned = dtree.check(pruned_tree, validation_set)
		if(perf_pruned >= best_tree_performance):
			best_tree = pruned_tree
			best_tree_performance = perf_pruned

	if(tree == best_tree):
		return best_tree
	else:
		return prune(best_tree, validation_set)

def errors(fractions, training_set,test_set):
	classification_errors = []
	classification_variance = []
	for fraction in fractions:
		errors = []	
		
		for i in range(0,100):
			monk1_train, monk1_val = partition(training_set,fraction)
			tree = dtree.buildTree(monk1_train,mdata.attributes) 

			pruned_tree = prune(tree, monk1_val)
			errors.append(dtree.check(pruned_tree,test_set))

		
		mean = numpy.mean(errors)
		spread = numpy.var(errors)
		classification_errors.append(mean)
		classification_variance.append(spread)


	return classification_errors, classification_variance

##print(ent())
##print(gain())
##bldTree()

fractions = [0.3,0.4,0.5,0.6,0.7,0.8]
monk1_errors, monk1_spread = errors(fractions,mdata.monk1,mdata.monk1test)
monk2_errors, monk2_spread = errors(fractions,mdata.monk2,mdata.monk2test)
monk3_errors, monk3_spread = errors(fractions,mdata.monk3,mdata.monk3test)


plt.plot(fractions,monk1_errors,'o--', fractions,numpy.add(monk1_errors,monk1_spread),'r--', fractions,numpy.subtract(monk1_errors,monk1_spread),'r--')
plt.xlabel('Fraction')
plt.ylabel('Performance')
plt.title('Classification performance on the test set for MONK-1')
plt.grid(True)
plt.show()


plt.plot(fractions,monk3_errors,'o--', fractions,numpy.add(monk3_errors,monk3_spread),'r--', fractions,numpy.subtract(monk3_errors,monk3_spread),'r--')
plt.xlabel('Fraction')
plt.ylabel('Performance')
plt.title('Classification performance on the test sets for MONK-3')
plt.grid(True)
plt.show()

plt.plot(fractions,monk2_errors,'o--', fractions,numpy.add(monk2_errors,monk2_spread),'r--', fractions,numpy.subtract(monk2_errors,monk2_spread),'r--')
plt.xlabel('Fraction')
plt.ylabel('Performance')
plt.title('Classification performance on the test sets for MONK-2')
plt.grid(True)
plt.show()
