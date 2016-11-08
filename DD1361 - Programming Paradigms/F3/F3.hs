-- Konstantin Sozinov

import F2
import Data.List
import Data.Ord
import Data.Function

-- I/O part of the program
 
main = do
        -- get all content from input as a single string
        content <- getContents
        -- create a list of MolSeq  
        let dna = createDNA content
        -- create distance matrix with distanceMetrix function from F2
            matrix = distanceMatrix dna
            inputTree = createInputTrees dna []
            tree = neighborJoining 1 inputTree matrix
            treeStr = printTree tree  
        putStrLn treeStr

-- This function creates a list of MolSeq from a single input string 
createDNA :: String -> [MolSeq]
createDNA content =
        -- create a list of one line strings from input
        -- i.e. separate the whole content string with a list of
        -- strings where one string is a one line from input
        let separatedContent = lines content
        -- parse separated content
        in createDNA' separatedContent []

-- Helper function for createDNA.
-- It will parse one line string and create one MolSeq
-- then it will append it to list of the MolSeq and recurse
-- with the other one line strings  
createDNA' :: [String] -> [MolSeq] -> [MolSeq]
-- base case -> when list of one line strings is over, return result
createDNA' [] dnas = dnas
createDNA' (x:xs) dnas =
    -- take all one line strings and split it with words function
    let s = words x
    -- get the head of s, i.e. first sequence name
        seqName = head s
    -- get the tail of s, i.e. first sequence sequence     
        seq = head $ tail s
    -- create a DNA with name and sequence
        dna = MolSeq DNA seqName seq
    -- place one DNA in list of DNAs and recurse with the rest of contents      
    in createDNA' xs (dnas++[dna])

-- This function will print out the evolution three with Newick format
printTree :: Tree String -> String
-- in case we found a root  
printTree (Root a t1 t2 t3) = "(" ++ printTree t1 ++ "," ++ printTree t2 ++ "," ++ printTree t3 ++ ")"
-- in case we found a empty node
printTree (Node a Nil Nil) = a
-- in case we found a node with two trees under
printTree (Node a t1 t2) = "(" ++ printTree t1 ++ "," ++ printTree t2 ++ ")"  

-- Logic part of the program

-- data type for a tree
data Tree a = Nil | Node a (Tree a) (Tree a) | Root a (Tree a) (Tree a) (Tree a) deriving (Show, Eq)

-- This function creates input trees for algorithms first iteration from the list of MolSeq. 
-- Every tree consists of one node.
createInputTrees :: [MolSeq] -> [Tree String] -> [Tree String]
-- empty molseq list -> return trees
createInputTrees [] trees = trees
createInputTrees (x:xs) trees=
        -- name of the node is sequence name
        let name = seqName x
        -- create Node with 2 empty childs
            node = Node name Nil Nil
        in createInputTrees xs (trees++[node])


neighborJoining :: Int -> [Tree String] -> [(String,String,Double)] -> Tree String
neighborJoining i t d
    -- so far amount of trees is greater then 3
    | f > 3 =
    -- compute s matrix    
        let s = selectionFunction f d d
        -- Find the lowest pair of tupples(It will minimize S matrix)
            (node1,node2,_) = lowest s
            -- create two nodes which will be a new tree    
            candidateNode1 = treeLookup t node1  
            candidateNode2 = treeLookup t node2
            -- create new tree
            newTreeName = ("T" ++ show i)
            newTree = Node newTreeName candidateNode1 candidateNode2
            -- delete old tree from the list of trees
            deleted = delete candidateNode2 $ delete candidateNode1 t
            --  append new tree to the list of trees
            updatedListOfTrees = newTree:deleted
            -- calculate new distance matrix
            newD = foldl (\acc n@(x,y,val) ->
            -- if x and y are the same as node1 and node2, collect same values from D 
                if x /= node1 && x /= node2 && y /= node1 && y /= node2 
                then n:acc
                -- recurse with accumulator if x or y is node2
                -- i.e. avoid to comupute new distance twice 
                else if x == node2 || y == node2  
                     then acc
                     -- if x and y are the same as node1, it is distance to tree itself (zero)
                     else if y == node1 && x == node1
                          then (newTreeName,newTreeName,0):acc
                          -- otherwise if x is the same as node1 
                          else if x == node1
                              -- calculate the sum of distances y to node1 and y to node2    
                               then (newTreeName,y,((distanceMatrixLookup y node1 d) + (distanceMatrixLookup y node2 d)) / 2):acc
                               -- if x are not the same as node1, calculate the sum of distanes from x to node1 and to node2
                               else (x,newTreeName,((distanceMatrixLookup x node1 d) + (distanceMatrixLookup x node2 d)) / 2):acc) [] d
        in  neighborJoining (i+1) updatedListOfTrees newD
    -- when we are done return the tree with 3 nodes
    | otherwise = (Root "T" (t !! 0) (t !! 1) (t !! 2)) 
    where f = length t    

-- 
distanceMatrixLookup :: String -> String -> [(String,String,Double)] -> Double
distanceMatrixLookup _ _ [] = 0
distanceMatrixLookup from to ((name1,name2,val):xs) =
    if (from == name1 && to == name2) || (from == name2 && to == name1)
    then val
    else distanceMatrixLookup from to xs  

-- This function helps to find one tree with its name
treeLookup :: [Tree String] -> String -> Tree String
treeLookup (n@(Node a _ _):xs) str =
    if a == str then n
    else treeLookup xs str  

-- This function finds the lowest tupple in the S matrix.
lowest :: [(String,String,Double)] -> (String,String,Double)
lowest s =
    -- sort the matrix by value
    let sorted = sortBy (compare `on` (\(_,_,val) -> val)) s
    -- the smallest will be head
    in head sorted

-- This function creates S matrix, first argument is an integer which tells
-- how many trees we have in current iteraction. Second argument and third arguments
-- are distance matrices. (Second is used like an accumulator). Output will be matrix S
-- for one iteration in the algorithm.
selectionFunction :: Int -> [(String,String,Double)] -> [(String,String,Double)] -> [(String,String,Double)]
-- We have looked at all values in distance matrix -> we are done
selectionFunction f [] d = []
selectionFunction f ((name1,name2,val):xs) d =
    -- avoid same molecules, distance for them are zero
    if name1 == name2 
        then selectionFunction f xs d
        -- calculate first term in the second part of the selection equation
        else let first = selectionFunction' name1 d
        -- calculate second term in the second part of the selection equation
                 second = selectionFunction' name2 d
                 -- calculate selection equation
                 select = (fromIntegral (f-2) * val) - (first + second)
                 -- append value to the results and recurse 
             in (name1, name2, select):selectionFunction f xs d     

-- Helper function for selection function. It takes one sequence name and sums distance 
-- from it to all other.
selectionFunction' :: String -> [(String,String,Double)] -> Double
-- when we looked att all values in distance matrix -> we are done
selectionFunction' a [] = 0
selectionFunction' a ((x,y, val):zs)
	--| a == x && a == y = selectionFunction' a zs
    -- check tupple, if we founded seeked name take value and recurse with rest
    | a == x = val + selectionFunction' a zs    
    | a == y = val + selectionFunction' a zs
   	| otherwise = selectionFunction' a zs

