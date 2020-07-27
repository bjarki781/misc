import Data.List (sort)
import System.Process (system)

data Cell = Dead | Live deriving (Eq, Ord)

instance Show Cell where
    show d = if d == Dead then "·" else "X"

type State = [[Cell]]
type Index = Int

newState :: State -> State
newState state = [[ newCell state i j 
                    | j <- [0..y]] | i <- [0..x]]
    where x = (length state) - 1
          y = (length $ head state) - 1

newCell :: State -> Index -> Index -> Cell
newCell state i j = if neighbours  == 3 then Live
                                else if neighbours == 2 && cell == Live 
                                then Live 
                                else Dead
    where cell = state !! i !! j 
          neighbours = length $ dropWhile (==Dead) $ sort nlist
          nlist = map relCell [(m,n) | m <- [-1..1], n <- [-1..1], (m,n) /= (0,0)]
          relCell coor = state !! ((i+fst coor) `mod` x) !! ((j+snd coor) `mod` y)
          x = (length state)
          y = (length $ head state)
         
   

doLoop :: State -> IO ()
doLoop state = do
    -- system "sleep 0.4; clear"
    display state
    display state
    doLoop (newState state)

display :: State -> IO ()
display state = do
    mapM_ showRow state
    where showRow row = putStrLn (concat $ map show row)


main = do
    doLoop orgState
    where orgState = map ((++) (replicate 15 Dead)) 
                     [ [ Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead ],
                       [ Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Live, Dead, Dead ],
                       [ Dead, Dead, Dead, Dead, Dead, Dead, Live, Dead, Live, Dead, Dead ],
                       [ Dead, Dead, Dead, Dead, Dead, Dead, Dead, Live, Live, Dead, Dead ],
                       [ Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead ],
                       [ Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead ],
                       [ Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead ],
                       [ Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead ],
                       [ Dead, Live, Dead, Live, Dead, Live, Dead, Live, Live, Live, Dead ],
                       [ Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead ],
                       [ Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead ],
                       [ Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead ],
                       [ Dead, Live, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead ],
                       [ Dead, Dead, Live, Live, Live, Live, Live, Live, Live, Dead, Dead ],
                       [ Dead, Dead, Dead, Live, Dead, Live, Dead, Live, Dead, Dead, Dead ],
                       [ Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead ],
                       [ Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead ],
                       [ Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead ],
                       [ Dead, Live, Live, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead ],
                       [ Dead, Live, Live, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead ],
                       [ Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead ],
                       [ Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead ],
                       [ Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead ],
                       [ Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead ],
                       [ Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead ],
                       [ Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead ] ]
