import Data.Matrix

main = do
    draw b
    draw $ a * b
    where b = matrix 2 8 $ \(i,j) -> (59*i+j+2 `mod` 11)
          a = matrix 2 2 $ \(i,j) -> i+j
    
draw :: Matrix Int -> IO ()
draw x = (prettyMatrix x)
