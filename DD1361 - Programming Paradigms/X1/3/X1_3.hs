

main = do
        -- get all content from input as a single string
        amountGames <- getLine
        content <- getContents
        let games = lines content
        in   
        putStrLn treeStr


play :: [String] -> Integer
play (x:xs) =
	if play' x == x then 


['-', 'o', 'o', '-']


test ('-':'o':'o':xs) = 
	let x1 = ('o':'-':'-': test xs)
		x2 = ('o':'-':'-': test xs)
	in  x1
test ('o':'o':'-':xs) = 
	('-':'-':'o': test xs)

makeoneStepRight ('o':'o':'-':xs) = '-':'-':'o':xs
makeoneStepRight (x:xs) = x : makeoneStepRight xs



play' :: [Char] -> [Char]
play' []  = []
play' ('-':'o':'o':xs) = play' ('o':'-':'-':xs)
play' ('o':'o':'-':xs) = play' ('-':'-':'o':xs)
play' (x:xs) = x:play' xs


play2 :: [Char] -> [Char]
play2 []  = []
play2 ('-':'o':'o':xs) = play2 ('o':'-':'-':xs)
play2 ('o':'o':'-':xs) = play2 ('-':'-':'o':xs)
play2 (a:b:xs) = a:b:play2 xs   

