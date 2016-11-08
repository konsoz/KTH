--Konstantin Sozinov

module F2 where
import Data.List
-- 2. Molekylära sekvenser

-- Uppgift 1.

-- some data types for DNA/Protein
data MolType = DNA | Protein deriving (Show, Eq)
data MolSeq  = MolSeq MolType String String deriving (Show)

-- Uppgift 2.

-- This function tells if a string is a DNA or a Protein

string2seq :: String -> String -> MolSeq
string2seq [] [] = error "You must enter a sequence and its name!"  
string2seq nam seq =
	-- filter out all chars that belongs to DNA
	let 
	fltr = filter (\x -> not $ elem x ['A','C','G','T']) seq -- $ used to avoiding parenthesis
	lng = length fltr
	-- if filtered list has length zero it is a DNA, protein otherwise	
	in if lng == 0 then MolSeq DNA nam seq else MolSeq Protein nam seq

-- Uppgift 3.

-- some basic pattern matchning..
seqName :: MolSeq -> String
seqName (MolSeq _ name _) = name

seqSequence :: MolSeq -> String
seqSequence (MolSeq _ _ seq) = seq

seqLength :: MolSeq -> Int
seqLength (MolSeq _ _ seq) = length seq

seqType :: MolSeq -> MolType 
seqType (MolSeq mol _ _) = mol


-- Uppgift 4.

-- This function calculates evolution distance

seqDistance :: MolSeq -> MolSeq -> Double
seqDistance (MolSeq DNA name1 seq1) (MolSeq DNA name2 seq2) =
	-- In case we are seeking distance for 2 DNAs 
	let 
	hammingDistance = seqDistance' seq1 seq2 0
	normalizedHammingDist = fromIntegral (hammingDistance) / fromIntegral (length seq1)
	in if normalizedHammingDist > 0.74 then 3.3
	else -3/4*log(1-4*normalizedHammingDist/3) -- Jukes-Cantor modell  
seqDistance (MolSeq Protein name1 seq1) (MolSeq Protein name2 seq2) =
	-- In case we are seeking distance for 2 Proteins
	let
	hammingDistance = seqDistance' seq1 seq2 0
	normalizedHammingDist = fromIntegral (hammingDistance) / fromIntegral (length seq1)
	in if normalizedHammingDist > 0.94 then 3.7
	else -19/20*log(1-20*normalizedHammingDist/19)   
seqDistance _ _ = error "Incompatable types" 

-- helper function that calculates Hamming distance between two sequences

seqDistance' :: String -> String -> Int -> Int
seqDistance' [] [] dist = dist
seqDistance' (x:xs) (y:ys) dist =
	-- add 1 to distance if one char from sequence x 
	-- is not the same that other char in sequence y
	if x /= y then seqDistance' xs ys (1+dist)
	else seqDistance' xs ys dist  

-- 3. Profiler och sekvenser

-- Uppgift 1.

-- data type for profile

data Profile = Profile [[(Char,Double)]] MolType Int String deriving (Show)

nucleotides = "ACGT"
aminoacids = sort "ARNDCEQGHILKMFPSTWYVX"

-- creates a C matrix, which is used in order to create a profile matrix M 
makeProfileMatrix :: [MolSeq] -> [[(Char,Int)]]
makeProfileMatrix [] = error "Empty sequence list"
makeProfileMatrix sl = res
  where 
    t = seqType (head sl)
    defaults = 
      if (t == DNA) then
      	-- create default list with nucleotides/aminoacids 
      	-- depending on type
        zip nucleotides (replicate (length nucleotides) 0)
      else 
        zip aminoacids (replicate (length aminoacids) 0)
    -- take out all sequences from the list of the MolSeq type
    strs = map seqSequence sl
    -- step 1: take the list of the sequences and transpose them
    -- i.e. if we have strs = ["ACATAA","AAGTCA","ACGTGC"] transpose
    -- will return ["AAA","CAC","AGG","TTT","ACG","AAC"]
    -- step 2: sort the trasnposed list
    -- step 3: group the sorted list i.e. a list with 
    -- ["AAA","ACG"] will be grouped into [["AAA"],["A","C","G"]]
    -- step 4: take out the head and calculate the length of the
    -- grouped list i.e. a list with [["AAA"],["A","C","G"]] would
    -- be [[('A',3)],[('A',1),('C',1),('G',1)]] 
    tmp1 = map (map (\x -> ((head x), (length x))) . group . sort)
               (transpose strs)
    -- equals function for comparing of two tupples with molecule
    equalFst a b = (fst a) == (fst b)
    -- replace "holes" with default list with unionBy i.e.
    -- if we have a tmp list : [[('A',3)],[('A',1),('C',1),('G',1)]]
    -- it well be transformed into 
    -- [[('A',3),('C',0),('G',0),('T',0)],[('A',1),('C',1),('G',1),('T',0)]]
    -- and sort if it is necessary  
    res = map sort (map (\l -> unionBy equalFst l defaults) tmp1)

-- Uppgift 2.

-- This function transforms a C matrix to profile matrix M
molseqs2profile :: String -> [MolSeq] -> Profile
molseqs2profile [] [] = error "Can't handle empty sequence with empty name!"
molseqs2profile name molseq =
	let
	c = makeProfileMatrix molseq -- create profile for the sequences
	lngth = length molseq -- calculate the length of the profile(amount of the sequences)
	typ = seqType (head molseq) -- get the type of every sequence 
	m = map ( map (\(x,y) -> (x,(fromIntegral y) / fromIntegral  (lngth)))) c
	-- take out every sequence and calculate every position in M matrix  
	in Profile m typ lngth name

-- Uppgift 3.

-- some basic pattern matching again..
profileName :: Profile -> String
profileName (Profile _ _ _ name) = name

profileType :: Profile -> MolType
profileType (Profile _ t _ _) = t

-- This function returns the relative frequency for one molecule in M matrix
profileFrequency :: Profile -> Int -> Char -> Double
profileFrequency (Profile m _ _ _ ) pos char =
	-- take out one column from the profile matrix
	let l = m !! pos
	in profileFrequency' l char 

profileFrequency' :: [(Char,Double)] -> Char -> Double
profileFrequency' (x:xs) char =
	-- seek for the right charachter, if founded return the relative frequency
	if fst x == char then snd x
	else profileFrequency' xs char

-- Uppgift 4.

-- This function calculates distance between two profile matrices 
profileDistance :: Profile -> Profile -> Double
profileDistance (Profile m1 _ _ _ ) (Profile m2 _ _ _ ) =
	profileDistance' m1 m2 	

profileDistance' :: [[(Char,Double)]] -> [[(Char,Double)]] -> Double
profileDistance' [] [] = 0
-- begin with the second column and so on..
profileDistance' ([]:zs) ([]:cs) = profileDistance' zs cs
profileDistance' ((x:y):zs) ((a:b):cs) =
	-- calculate the absolute difference between first column in first
	-- matrix and first column in second matrix 
	let
	diff = abs (snd (x) - snd (a))
	-- recurse with the diff and calculate the absolute difference
	-- for other tupples in this column
	in diff + profileDistance' (y:zs) (b:cs)

-- 4. Generell beräkning av avståndsmatriser

-- Uppgift 1

-- typeclass for evolution
class Evol evol where
	-- name for DNA/Protein
	name :: evol -> String
	-- distance for sequence/profile
	distance :: evol -> evol -> Double
	-- type for MolSeq/Profile
	typ :: evol -> MolType

	-- This function will calculate all distances for a list of MolSeq/Profile
	-- and make a list of tupples with (name1,name2,distance)
	distanceMatrix :: [evol] -> [(String,String,Double)]
	distanceMatrix [] = []
	distanceMatrix (x:xs) =
		-- calculate distance from one MolSeq/Profile to all other 
		let d = distanceMatrix' x (x:xs)
		-- recurse with returned tupple and the rest of MolSeqs/Profiles list
		in d ++ distanceMatrix xs

	--helper function that calculates distance from one MolSeq/Profile to all other
	-- in the list of MolSeqs/Profiles	
	distanceMatrix' :: evol -> [evol] -> [(String,String,Double)]
	distanceMatrix' ev [] = []
	distanceMatrix' ev (e:es) = (name ev, name e, distance ev e):distanceMatrix' ev es 

-- making our types instances of typeclasse evol
instance Evol MolSeq where
	typ = seqType
	distance = seqDistance
	name = seqName

instance Evol Profile where
	typ = profileType
	distance = profileDistance
	name = profileName