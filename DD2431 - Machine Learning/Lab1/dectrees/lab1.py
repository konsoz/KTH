import monkdata as mdata
import dtree as dtree
import drawtree_qt5 as draw

## Assignment 0:
## 
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
	return (('entropy1',entropymonk1),('entropy2',entropymonk2),('entropy3',entropymonk3))


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
## 
## 
## Assignment 6: 
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
	draw.drawTree(tree_monk1)


print(ent())
print(gain())
bldTree()