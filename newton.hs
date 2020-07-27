deriv f c = (f (c+h) - f c) / h
            where h = 0.00000001

newton 0 f x = x
newton n f x = xmark - f xmark / deriv f xmark
            where xmark = newton (n-1) f x

main = do
    print $ newton 5 (\x-> x*1.10**x-5) 3
