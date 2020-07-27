{-# LANGUAGE ParallelListComp #-}


simpson :: Int -> (Double -> Double) -> Double -> Double -> Double
simpson n f a b = delta/3 * (f a + f b + sum body)
    where delta = (b-a)/(fromIntegral n)
          body = [t*f c | c<-[a+delta,a+delta+delta.. b-delta] | t<-cycle [4,2] ]

main = do
    print $ simpson 1000000 (acos) (-1) 0

