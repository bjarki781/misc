main :: IO ()
main = do 
    putStrLn $ (show . sum) rod
    --where rod = [ ( ((2/3)^^n)/((1/2)^^n+(9/10)^^n)) | n <- [0..1000]]
    where rod = [ (2/3)^^n / ((9/10)^^n + (1/2)^^n) | n <- [0..1000]]
