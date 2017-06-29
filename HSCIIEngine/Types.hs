module HSCIIEngine.Types where

type Dimensions = Vec2 Int
type Height = Int
type Width = Int

data Vec2 a = V2 a a deriving (Eq)
type V2F = Vec2 Float
type X = Float
type Y = Float

type Row = String
type Image = [String]

type Object = (V2F, Dimensions, Image)

v2fzero = (V2 0 0) :: V2F

v2op :: (a -> b -> c) -> Vec2 a -> Vec2 b -> Vec2 c
v2op f (V2 a1 a2) (V2 b1 b2) = (V2 (f a1 b1) (f a2 b2))

instance Num a => Num (Vec2 a) where
   (+) = v2op (+)
   (-) = v2op (-)
   (*) = v2op (*)
   abs    (V2 a b) = (V2 (abs a) (abs b))
   signum (V2 a b) = (V2 (signum a) (signum b))
   fromInteger i = (V2 (fromInteger i) (fromInteger i))

instance Functor Vec2 where
  fmap f (V2 a b) = (V2 (f a) (f b))
