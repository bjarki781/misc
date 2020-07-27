import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams(toFile)

fg :: Double -> Double -> Double
fg n th = sum sins 
    where sins = [(sin (i*th)*(sin (i*th))/i) | i<-[1,3..n]]

wave :: [Double] -> [(Double, Double)]
wave ths = [ (th, fg 300 th) | th<-ths]

main = toFile def "fourier.svg" $ do
    layout_title .= "Square wave"
    setColors [opaque black, opaque blue]
    plot (line "Square" [wave [-10,-9.95..10]])


