import Data.Number

deriv :: (BigFloat -> BigFloat) -> BigFloat -> BigFloat
deriv f c = (f (c+h) - f c) / h
    where h = 0.001

derivs :: Int -> (BigFloat -> BigFloat) -> (BigFloat -> BigFloat)
derivs n = foldr (.) (id) (take n $ repeat deriv)

main = do
    print $ derivs 4 (\x->x**7) 1

