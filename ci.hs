















7. Lítið Haskell forrit sem spýtir út úr sér alla mögulegar lausnir á dulkóðuninni. Leitað handvirkt í gegnum þær með grep the

-- ci.hs
toLetter n = toEnum (n + 97)
fromLetter c = fromEnum c - 97

g :: Int -> Int -> Char -> Char
g a' b' c = toLetter ( a'*(n - b') `mod` 26 )
    where n = fromLetter c
 
m :: String -> [String]
m c = [ str a' b' c | a' <- as, b' <- bs]
      where as = [1,3..25]
            bs = [0..25]

str :: Int -> Int -> String -> String
str a' b' c = show a' ++ " " ++ show b' ++ ": " ++ map (g a' b') c
	 where abbr = take 50 c

main = do
    c <- readFile "ciphertext.txt"
    mapM_ print (m c)


> ./ci | grep the
...
alan turing was a brilliant british mathematician
...
