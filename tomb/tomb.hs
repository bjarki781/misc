import Linear
import Linear.Metric
import Data.Maybe
import Data.List
import Codec.Picture
import Codec.Picture.Types

type Vec = V3 Double
data Ray = Ray {r_origin :: Vec, r_direction :: Vec}
data Object = Object {o_isInside :: Vec -> Bool, o_surface :: (V2 Double) -> Double}
data Light = Light {l_shape :: Object, l_intensity :: Double}
type Thing = Either Object Light
type World = [Thing]

raytip :: Ray -> Vec
raytip (Ray o d) = o + d

instance Show Object where
    show _ = "object"

instance Show Light where
    show (Light _ l_intensity) = "light with intensity " ++ show l_intensity


t_isInside :: Thing -> Vec -> Bool
t_isInside (Left o) v = (o_isInside o) v
t_isInside (Right l) v = (o_isInside (l_shape l)) v


screen_width = 300 :: Int
screen_height = 300 :: Int
sphere :: Double -> Vec -> Vec -> Bool
sphere r (V3 x1 y1 z1) (V3 x y z) = (x-x1)^2 + (y-y1)^2 + (z-z1)^2 < r^2

sphere_surface :: Double -> Vec -> V2 Double -> Double
sphere_surface r (V3 x1 y1 z1) (V2 x y) = sqrt (r^2 - (x-x1)^2 + (y-y1)^2) + z1

nabla :: ((V2 Double) -> Double) -> (V2 Double) -> (Vec, Vec)
nabla f (V2 x0 y0) = ((V3 1 0 xtangent), (V3 0 1 ytangent))
    where xtangent = derive (\x -> (f (V2 x y0))) x0
          ytangent = derive (\y -> (f (V2 x0 y))) y0


obj = Object (sphere 0.5 (V3 0.1 1 0.2)) (sphere_surface 0.5 (V3 0.1 1 0.2))
sun = Light (Object (sphere 0.1 (V3 (-0.7) 0.8 (-0.7))) (sphere_surface 0.1 (V3 (-0.5) 0.8 (-0.4)))) 2.0

world = [Left obj, Right sun]
 
raytrace :: Int -> Int -> PixelRGB8
raytrace x y = PixelRGB8 val val val
    where val = if isNothing thing then 0 else findValue (fromJust thing) ((fromJust vec) - raytip ray)
          (thing, vec) = rayCollidor (Ray (r_origin ray) (normalize $ r_direction ray)) world
          ray = pixelToRay x y

findValue :: Thing -> Vec -> Pixel8
findValue (Left obj) vec = toPixelWord $ 255 - 120*(norm vec)
findValue (Right lgt) vec = toPixelWord $ 255 - 60*(1/l_intensity lgt)*(norm vec)

toPixelWord :: Double -> Pixel8
toPixelWord d = fromIntegral $ floor $ t
    where t = min 255 (max 0 d)

derive :: (Double -> Double) -> Double -> Double
derive f x = (f (x+h) - f x) / h where h = 0.0001

(!) :: [a] -> Maybe Int -> Maybe a
(!) xs Nothing = Nothing
(!) xs (Just i) = Just (xs !! i)

pixelToRay :: Int -> Int -> Ray
pixelToRay x y = Ray o (d-o)
    where o = zero
          d = V3 (-0.5 + fromIntegral x / fromIntegral screen_height) 0.3 (0.5 - fromIntegral y / fromIntegral screen_height)

-- given a ray and the world return whether we hit anything and if so where do we hit
rayCollidor :: Ray -> World -> (Maybe Thing, Maybe Vec)
rayCollidor r w = (w ! thing_hit, loc) 
    where all_collisions = map (\t -> map (t_isInside t) ray_pieces) w
          ray_pieces = [r_origin r + s * r_direction r | s <- map fromRational [0,0.05..3]]
          hits = map or all_collisions
          thing_hit = elemIndex True hits
          thing_hit_bools = all_collisions ! thing_hit
          loc = if isNothing thing_hit then zero else ray_pieces ! (elemIndex True (fromJust thing_hit_bools))

--reflect :: Thing -> Vec -> Vec -> Vec
--reflect thing loc to = from
--    where from

-- begin game definitions
main :: IO ()
main = do
    print $ nabla (sphere_surface 1 (V3 0 0 0)) (V2 0.2 0)
    writePng "tomb.png" $ generateImage raytrace screen_width screen_height

