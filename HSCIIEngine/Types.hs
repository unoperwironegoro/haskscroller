module HSCIIEngine.Types where

type Dimensions = Vec2 Int
type Height = Int
type Width = Int

data Vec2 a = V2 a a deriving (Eq, Show, Read)
type V2F = Vec2 Float
type X = Float
type Y = Float

type Row = String
type Image = [String]

type Object = (V2F, Dimensions, Image)

v2fzero = (V2 0 0) :: V2F

v2op :: (a -> b -> c) -> Vec2 a -> Vec2 b -> Vec2 c
v2op f (V2 a1 a2) (V2 b1 b2) = (V2 (f a1 b1) (f a2 b2))

flipY :: (Num a) => Vec2 a -> Vec2 a
flipY (V2 x y) = (V2 x (-y))

flipX :: (Num a) => Vec2 a -> Vec2 a
flipX (V2 x y) = (V2 (-x) y)

below :: Ord a => Vec2 a -> Vec2 a -> Bool
below (V2 _ y1) (V2 _ y2) = y1 > y2

above :: Ord a => Vec2 a -> Vec2 a -> Bool
above (V2 _ y1) (V2 _ y2) = y1 < y2

leftOf :: Ord a => Vec2 a -> Vec2 a -> Bool
leftOf (V2 x1 _) (V2 x2 _) = x1 < x2

rightOf :: Ord a => Vec2 a -> Vec2 a -> Bool
rightOf (V2 x1 _) (V2 x2 _) = x1 > x2

clamp :: Ord a => Vec2 a -> Vec2 a -> Vec2 a -> Vec2 a
clamp (V2 x y) (V2 xmin ymin) (V2 xmax ymax)
  = V2 (clamp' xmin xmax x) (clamp' ymin ymax y)
  where
    clamp' mn mx = (max mn) . (min mx)

instance Num a => Num (Vec2 a) where
   (+) = v2op (+)
   (-) = v2op (-)
   (*) = v2op (*)
   abs    (V2 a b) = (V2 (abs a) (abs b))
   signum (V2 a b) = (V2 (signum a) (signum b))
   fromInteger i = (V2 (fromInteger i) (fromInteger i))

instance Functor Vec2 where
  fmap f (V2 a b) = (V2 (f a) (f b))
